#%%
# 필요한 패키지 로드
required_packages <- c("jsonlite", "dplyr", "purrr", "stringr", 
                       "ggplot2", "tidyr", "broom", "readr", 
                       "stats", "forcats", "car", "gridExtra", "progressr","tictoc")

install.packages(required_packages)
lapply(required_packages, library, character.only = TRUE)
#%%
getwd()
setwd("C:/Users/insung/Desktop/5-1/R프로그래밍/")
# 폴더 경로 설정
matches_folder_path <- "C:/Users/insung/Desktop/5-1/R프로그래밍/data/matches"
events_folder_path <- "C:/Users/insung/Desktop/5-1/R프로그래밍/기말프로젝트/events"
#%%
# 1. matches 폴더내 라리가 경기 데이터프레임 생성
match_json_files <- list.files(matches_folder_path, pattern = "\\.json$", full.names = TRUE)

matches_info_list <- lapply(match_json_files, function(f) {
  data <- fromJSON(f, flatten = TRUE)
  if(is.data.frame(data)) {
    return(data)
  } else if(is.list(data)){
    return(bind_rows(data))
  } else {
    return(NULL)
  }
})

matches_df <- bind_rows(matches_info_list)
# La Liga 경기만 필터
laliga_matches_df <- matches_df %>% 
  filter(competition.competition_name == "La Liga")

# 전체 경기 수
total_laliga_matches <- nrow(laliga_matches_df)
cat("La Liga 전체 경기 수:", total_laliga_matches, "\n")

# 승리/패배 정보 파악
laliga_matches_df <- laliga_matches_df %>%
  mutate(
    Result = case_when(
      home_score > away_score ~ "Home Win",
      home_score < away_score ~ "Away Win",
      TRUE ~ "Draw"
    )
  )

# 승패 경기만 필터 (무승부 제외)
laliga_played_no_draw <- laliga_matches_df %>% 
  filter(Result %in% c("Home Win", "Away Win"))

# 승리 팀, 패배 팀 컬럼 추가
laliga_played_no_draw <- laliga_played_no_draw %>%
  mutate(
    winner_team = ifelse(Result == "Home Win", `home_team.home_team_name`, `away_team.away_team_name`),
    loser_team = ifelse(Result == "Home Win", `away_team.away_team_name`, `home_team.home_team_name`)
  )

# 이벤트 파일 목록 가져오기
event_files <- list.files(events_folder_path, pattern = "\\.json$", full.names = TRUE)

# La Liga 경기의 match_id 리스트
laliga_match_ids <- laliga_matches_df$match_id

# La Liga 경기와 매칭되는 이벤트 파일 필터
laliga_event_files <- event_files[basename(event_files) %in% paste0(laliga_match_ids, ".json")]

cat("La Liga 이벤트 파일 수:", length(laliga_event_files), "\n")


######################## Define Function ########################
#################################################################
#%%
assign_zone <- function(x_coordinate, field_length = 120) {
  ifelse(x_coordinate <= field_length / 3, "Defensive",
         ifelse(x_coordinate <= (2 * field_length / 3), "Midfield", "Attacking"))
}

process_pass_events <- function(events, home_team, away_team) {
  ev_df <- as_tibble(events)
  
  if("location" %in% names(ev_df) && is.list(ev_df$location)) {
    ev_df <- ev_df %>%
      unnest_wider(location, names_sep = "_") %>%
      rename(x = location_1, y = location_2)
  }
  
  pass_events <- ev_df %>%
    filter(type.name == "Pass", !is.na(x), !is.na(y))
  
  pass_events <- pass_events %>%
    mutate(zone = assign_zone(x, field_length = 120))
  
  home_first <- pass_events %>% filter(possession_team.name == home_team, period == 1)
  home_second <- pass_events %>% filter(possession_team.name == home_team, period == 2)
  away_first <- pass_events %>% filter(possession_team.name == away_team, period == 1)
  away_second <- pass_events %>% filter(possession_team.name == away_team, period == 2)
  
  return(list(home_first = home_first, home_second = home_second, away_first = away_first, away_second = away_second))
}

calculate_spatial_possession <- function(home_df, away_df){
  zones <- c("Defensive", "Midfield", "Attacking")
  home_counts <- sapply(zones, function(z) sum(home_df$zone == z))
  away_counts <- sapply(zones, function(z) sum(away_df$zone == z))
  
  home_total <- sum(home_counts)
  away_total <- sum(away_counts)
  
  tibble(
    Zone = zones,
    Home = home_counts,
    Away = away_counts,
    Home_Perc = if(home_total > 0) (home_counts / home_total)*100 else 0,
    Away_Perc = if(away_total > 0) (away_counts / away_total)*100 else 0
  )
}




################################## Main Code ##################################
###############################################################################
#%%
# 모든 승패 경기에 대해 점유율 계산 및 기록
possession_records <- list()
error_files <- c()

for(i in seq_len(nrow(laliga_played_no_draw))) {
  match_info <- laliga_played_no_draw[i,]
  
  target_match_id <- match_info$match_id
  home_team <- match_info$`home_team.home_team_name`
  away_team <- match_info$`away_team.away_team_name`
  winner <- match_info$winner_team
  loser <- match_info$loser_team
  
  file_name <- paste0(target_match_id, ".json")
  file_path <- file.path(events_folder_path, file_name)
  
  if(!file.exists(file_path)) {
    error_files <- c(error_files, file_name)
    next
  }
  
  events <- fromJSON(file_path, flatten = TRUE)
  if(!is.data.frame(events)) {
    events <- bind_rows(events)
  }
  
  pass_data <- process_pass_events(events, home_team, away_team)
  
  combined_home <- bind_rows(pass_data$home_first, pass_data$home_second)
  combined_away <- bind_rows(pass_data$away_first, pass_data$away_second)
  
  spatial_df <- calculate_spatial_possession(combined_home, combined_away)
  
  # 승리팀/패배팀 기준으로 재정렬
  winner_is_home <- (winner == home_team)
  
  spatial_df <- spatial_df %>%
    mutate(
      Winner_Perc = if(winner_is_home) Home_Perc else Away_Perc,
      Loser_Perc = if(winner_is_home) Away_Perc else Home_Perc
    )
  
  possession_record <- spatial_df %>%
    select(Zone, Winner_Perc, Loser_Perc) %>%
    pivot_wider(names_from = Zone, values_from = c(Winner_Perc, Loser_Perc))
  
  possession_record <- possession_record %>% 
    mutate(match_id = target_match_id, winner_team = winner, loser_team = loser)
  
  possession_records[[length(possession_records)+1]] <- possession_record
}

possession_df <- bind_rows(possession_records)
possession_df

#%%
# 통계 검정 함수 정의
perform_statistical_tests <- function(df, zones) {
  results <- list()
  
  for(z in zones) {
    winner_col <- paste0("Winner_Perc_", z)
    loser_col <- paste0("Loser_Perc_", z)
    
    winner_data <- df[[winner_col]]
    loser_data <- df[[loser_col]]

    # 정규성 검정
    shapiro_winner <- shapiro.test(winner_data)
    shapiro_loser <- shapiro.test(loser_data)
    
    normal_winner <- (shapiro_winner$p.value > 0.05)
    normal_loser <- (shapiro_loser$p.value > 0.05)
    
    # 등분산성 검정
    if(normal_winner && normal_loser) {
      levene_res <- leveneTest(y = c(winner_data, loser_data), 
                               group = rep(c("Winner","Loser"), 
                                           c(length(winner_data), length(loser_data))))
      equal_var <- (levene_res$`Pr(>F)`[1] > 0.05)
    } else {
      equal_var <- FALSE
    }
    
    # 검정 선택
    if(normal_winner && normal_loser) {
      if(equal_var) {
        test_res <- t.test(winner_data, loser_data, var.equal = TRUE)
        test_type <- "Independent T-Test (equal var)"
      } else {
        test_res <- t.test(winner_data, loser_data, var.equal = FALSE)
        test_type <- "Welch T-Test"
      }
    } else {
      test_res <- wilcox.test(winner_data, loser_data, paired = FALSE, alternative = "two.sided")
      test_type <- "Mann-Whitney U"
    }
    
    results[[length(results)+1]] <- tibble(
      Zone = z,
      Test = test_type,
      Statistic = test_res$statistic,
      p_value = test_res$p.value
    )
  }
  
  results_df <- bind_rows(results)
  if(nrow(results_df) > 0) {
    results_df <- results_df %>%
      mutate(Corrected_p = p.adjust(p_value, method = "bonferroni")) %>%
      mutate(Significant = Corrected_p < 0.05)
  }
  
  return(results_df)
}
#%%
zones <- c("Defensive","Midfield","Attacking")
stat_results <- perform_statistical_tests(possession_df, zones)
cat("\n통계 검정 결과:\n")
print(stat_results)
#%%

# 승리팀 vs 패배팀 구역별 점유율 박스 플롯
for (z in zones) {
  winner_col <- paste0("Winner_Perc_", z)
  loser_col <- paste0("Loser_Perc_", z)
  
  # 데이터 준비
  plot_df <- possession_df %>%
    select(match_id, winner_team, loser_team, !!winner_col, !!loser_col) %>%
    pivot_longer(cols = c(!!winner_col, !!loser_col), names_to = "Type", values_to = "Perc") %>%
    mutate(Type = ifelse(str_detect(Type, "Winner"), "Winner", "Loser"))
  
  # ggplot 객체 생성 (승리팀과 패배팀 색상 지정)
  p <- ggplot(plot_df, aes(x = Type, y = Perc, fill = Type)) +
    geom_boxplot() +
    scale_fill_manual(values = c("Winner" = "#1f77b4", "Loser" = "#ff7f0e")) +  # 색상 지정
    labs(title = paste(z, "Zone Possession Percentage: Winner vs Loser"), 
         y = "Possession Percentage", x = "Team Type") +
    theme_minimal() +
    theme(legend.position = "none")  # 범례 제거
  
  # 플롯을 리스트에 저장
  plot_list[[z]] <- p
}

#%%
# 저장된 플롯 확인
plot_list[[zones[1]]]  
#%%
plot_list[[zones[2]]]
#%%
plot_list[[zones[3]]]

#%%

# 총 패스 개수 카운팅
counting_pass <- function(match_id, team_name, events_folder_path) {
  file_path_event <- file.path(events_folder_path, paste0(match_id, ".json"))

  events <- fromJSON(file_path_event, flatten = TRUE)
  match_info <- matches_df %>% filter(match_id == as.integer(match_id)) %>% slice(1)
  
  # type.name이 "Pass"인 이벤트만 필터링
  team_passes <- events %>%
    filter(type.name == "Pass", team.name == team_name)
    
  pass_count <- nrow(team_passes)
  return(pass_count)
}
#%%
# 패스 위치(x,y) 추출 함수
create_pass_location_data <- function(match_id, team_name, events_folder_path) {
  file_path_event <- file.path(events_folder_path, paste0(match_id, ".json"))

  events <- fromJSON(file_path_event, flatten = TRUE)
  match_info <- matches_df %>% filter(match_id == as.integer(match_id)) %>% slice(1)
  
  # type.name이 "Pass"인 이벤트만 필터링
  team_passes <- events %>%
    filter(type.name == "Pass", team.name == team_name)
  pass_location <- data.frame(x = numeric(), y = numeric())
  
  for (idx in seq_len(nrow(team_passes))) {
    event <- team_passes[idx,]
    passer <- event$player.name
    recipient <- event$pass.recipient.name

   # 패스 시작점 추출
   if(!is.na(passer) &&
    !is.null(event$location) && length(event$location) == 1 && length(event$location[[1]]) == 2) {
    x_start <- event$location[[1]][1]
    y_start <- event$location[[1]][2]
    pass_location <- rbind(pass_location, data.frame(x = x_start, y = y_start))
  }
  
   # 패스 종점 추출
   if(!is.na(recipient) && 
    !is.null(event$pass.end_location) && length(event$pass.end_location) == 1 && length(event$pass.end_location[[1]]) == 2) {
    x_end <- event$pass.end_location[[1]][1]
    y_end <- event$pass.end_location[[1]][2]
    pass_location <- rbind(pass_location, data.frame(x = x_end, y = y_end))
   }
 }
  
  return(pass_location)
}


# 축구장 레이아웃 그리는 함수
draw_pitch <- function(field_length = 120, field_width = 80) {
  ggplot() +
    # 필드 배경
    geom_rect(aes(xmin=0, xmax=field_length, ymin=0, ymax=field_width), 
              fill="palegreen3") +
    # 중앙선
    geom_segment(aes(x=field_length/2, xend=field_length/2, y=0, yend=field_width)) +
    # 센터 서클 (반지름 10)
    annotate("path",
             x=field_length/2 + 10*cos(seq(0,2*pi,length.out=100)),
             y=field_width/2 + 10*sin(seq(0,2*pi,length.out=100)),
             color="black") +
    # 왼쪽 페널티 박스
    geom_rect(aes(xmin=0, xmax=18, ymin=(field_width/2)-20.15, ymax=(field_width/2)+20.15),
              fill=NA, color="black") +
    # 왼쪽 골박스
    geom_rect(aes(xmin=0, xmax=6, ymin=(field_width/2)-9.16, ymax=(field_width/2)+9.16),
              fill=NA, color="black") +
    # 왼쪽 골대
    geom_segment(aes(x=0, xend=0, y=(field_width/2)-3.66, yend=(field_width/2)+3.66), 
                 color="black", size=1.2) +
    # 오른쪽 페널티 박스
    geom_rect(aes(xmin=field_length-18, xmax=field_length, ymin=(field_width/2)-20.15, ymax=(field_width/2)+20.15),
              fill=NA, color="black") +
    # 오른쪽 골박스
    geom_rect(aes(xmin=field_length-6, xmax=field_length, ymin=(field_width/2)-9.16, ymax=(field_width/2)+9.16),
              fill=NA, color="black") +
    # 오른쪽 골대
    geom_segment(aes(x=field_length, xend=field_length, y=(field_width/2)-3.66, yend=(field_width/2)+3.66), 
                 color="black", size=1.2) +
    # 외곽선
    geom_rect(aes(xmin=0, xmax=field_length, ymin=0, ymax=field_width), fill=NA, color="black") +
    coord_fixed() +
    theme_void()
}

######################## One Match Pass Counting ##############################
#########################################################################
#%%

# 패스 수 카운팅 함수
counting_pass <- function(match_id, team_name, events_folder_path) {
  file_path_event <- file.path(events_folder_path, paste0(match_id, ".json"))
  events <- fromJSON(file_path_event, flatten = TRUE)
  
  # 패스 이벤트 필터링
  team_passes <- events %>%
    filter(type.name == "Pass", team.name == team_name)
  return(nrow(team_passes))
}

# 특정 경기 설정
example_match_id <- laliga_played_no_draw$match_id[1]  # 예시로 첫 번째 경기
winner_team <- laliga_played_no_draw$winner_team[laliga_played_no_draw$match_id == example_match_id][1]
loser_team <- laliga_played_no_draw$loser_team[laliga_played_no_draw$match_id == example_match_id][1]

# 승리팀과 패배팀의 패스 수 계산
winner_pass_count <- counting_pass(as.character(example_match_id), winner_team, events_folder_path)
loser_pass_count <- counting_pass(as.character(example_match_id), loser_team, events_folder_path)

# 데이터 준비
match_pass_data <- data.frame(
  team = c("Winner", "Loser"),
  pass_count = c(winner_pass_count, loser_pass_count),
  is_winner = c(1, 0) 
)

print("패스 수 데이터:")
print(match_pass_data)

# T-test 수행
t_test_result <- t.test(pass_count ~ team, data = match_pass_data)
print("T-test 결과:")
print(t_test_result)

# 로지스틱 회귀 분석
logistic_model <- glm(is_winner ~ pass_count, data = match_pass_data, family = "binomial")
summary(logistic_model)

# 오즈비 계산
odds_ratio <- exp(coef(logistic_model)["pass_count"])
print(paste("패스 수가 1단위 증가할 때 승리 확률의 오즈비:", round(odds_ratio, 3)))


######################## All Team Pass Counting ########################
########################################################################
#%%
# 모든 경기에서 승리팀과 패배팀의 패스 수 수집

pass_count_data <- list()
handlers("progress")  # 진행 상황 핸들러 활성화
tic("전체 작업 소요 시간")  # 전체 타이머 시작

with_progress({
  p <- progressor(steps = nrow(laliga_played_no_draw))
  
  for (i in 1:nrow(laliga_played_no_draw)) {
    # 진행 상황 업데이트
    p(sprintf("Processing match %d of %d", i, nrow(laliga_played_no_draw)))
    
    match_id <- laliga_played_no_draw$match_id[i]
    winner_team <- laliga_played_no_draw$winner_team[i]
    loser_team <- laliga_played_no_draw$loser_team[i]
    
    # 승리팀과 패배팀의 패스 수 계산
    winner_pass <- counting_pass(as.character(match_id), winner_team, events_folder_path)
    loser_pass <- counting_pass(as.character(match_id), loser_team, events_folder_path)
    
    # 데이터 저장
    pass_count_data[[i]] <- data.frame(
      match_id = match_id,
      team = c("Winner", "Loser"),
      pass_count = c(winner_pass, loser_pass),
      is_winner = c(1, 0)
    )
  }
})

toc()  # 전체 타이머 종료

# 데이터 병합
final_data <- bind_rows(pass_count_data)
print("최종 패스 수 데이터:")
print(head(final_data))

#%%
# T-test 수행 (승리팀 vs 패배팀 패스 수 평균 비교)
t_test_result <- t.test(pass_count ~ team, data = final_data)
print("T-test 결과:")
print(t_test_result)

# 로지스틱 회귀 분석
print("로지스틱 회귀 분석 결과:")
logistic_model <- glm(is_winner ~ pass_count, data = final_data, family = "binomial")
summary(logistic_model)

# 오즈비 계산
odds_ratio <- exp(coef(logistic_model)["pass_count"])
print(paste("패스 수가 1단위 증가할 때 승리 확률의 오즈비:", round(odds_ratio, 3)))


######################## One Match Pass Heat Map ########################
#########################################################################
#%%
# 예시: 특정 match_id에 대해 승리팀/패배팀 히트맵 시각화
if(nrow(laliga_played_no_draw) > 0) {
  example_match_id <- laliga_played_no_draw$match_id[1]  # 예시로 첫 번째 경기
  winner_team <- laliga_played_no_draw$winner_team[laliga_played_no_draw$match_id == example_match_id][1]
  loser_team <- laliga_played_no_draw$loser_team[laliga_played_no_draw$match_id == example_match_id][1]



  winner_pass_loc <- create_pass_location_data(as.character(example_match_id), winner_team,
                                               events_folder_path)
  winner_pass_loc <- winner_pass_loc %>%
    mutate(match_id = example_match_id, team_type = "Winner")

  loser_pass_loc <- create_pass_location_data(as.character(example_match_id), loser_team,
                                              events_folder_path)

  loser_pass_loc <- loser_pass_loc %>%
  mutate(match_id = example_match_id, team_type = "Loser")
}

#%%

# 승리팀 히트맵
winner_heatmap <- draw_pitch() +
  stat_bin2d(data = winner_pass_loc, aes(x = x, y = y, fill = ..count..), bins = 8, alpha = 0.8) +
  scale_fill_viridis_c(name = "Pass Count") +
  labs(title = "Winner Pass Heatmap", x = "Field Length", y = "Field Width") +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    legend.position = "right"
  )

# 패배팀 히트맵
loser_heatmap <- draw_pitch() +
  stat_bin2d(data = loser_pass_loc, aes(x = x, y = y, fill = ..count..), bins = 8, alpha = 0.8) +
  scale_fill_viridis_c(name = "Pass Count") +
  labs(title = "Loser Pass Heatmap", x = "Field Length", y = "Field Width") +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    legend.position = "right"
  )

# 두 개의 그래프를 나란히 출력
grid.arrange(winner_heatmap, loser_heatmap, ncol = 2)



######################## All Team Pass Heat Map ########################
########################################################################
#%%
# 모든 경기에서 승리팀과 패배팀의 패스 데이터를 저장할 리스트
all_winner_data <- list()
all_loser_data <- list()

# 전체 작업 타이머 시작
tic("전체 작업 소요 시간")

# 진행 바 초기화 (남은 시간 예측 기능 포함)
total_matches <- length(unique(laliga_played_no_draw$match_id))
pb <- progress_bar$new(
  format = "  진행률 [:bar] :percent 남은시간: :eta",
  total = total_matches,
  clear = FALSE, 
  width = 60
)

# laliga_played_no_draw 데이터의 모든 match_id를 순회
for (example_match_id in unique(laliga_played_no_draw$match_id)) {
  # 진행 바 업데이트
  pb$tick()
  
  # 승리팀과 패배팀 이름 추출
  winner_team <- laliga_played_no_draw$winner_team[laliga_played_no_draw$match_id == example_match_id][1]
  loser_team <- laliga_played_no_draw$loser_team[laliga_played_no_draw$match_id == example_match_id][1]
  
  # 승리팀 패스 위치 데이터 생성
  winner_pass_loc <- create_pass_location_data(as.character(example_match_id), winner_team, events_folder_path)
  winner_pass_loc <- winner_pass_loc %>%
    mutate(match_id = example_match_id, team_type = "Winner")
  
  # 패배팀 패스 위치 데이터 생성
  loser_pass_loc <- create_pass_location_data(as.character(example_match_id), loser_team, events_folder_path)
  loser_pass_loc <- loser_pass_loc %>%
    mutate(match_id = example_match_id, team_type = "Loser")
  
  # 리스트에 추가
  all_winner_data[[as.character(example_match_id)]] <- winner_pass_loc
  all_loser_data[[as.character(example_match_id)]] <- loser_pass_loc
}

# 전체 작업 소요 시간 종료
toc()

# 모든 경기의 데이터 병합
winner_data <- do.call(rbind, all_winner_data)
loser_data <- do.call(rbind, all_loser_data)
# 승리팀 전체 히트맵
winner_heatmap <- draw_pitch() +
  stat_bin2d(data = winner_data, aes(x = x, y = y, fill = ..count..), bins = 8, alpha = 0.8) +
  scale_fill_viridis_c(name = "Pass Count") +
  labs(title = "Overall Winner Pass Heatmap", x = "Field Length", y = "Field Width") +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    legend.position = "right"
  )

# 패배팀 전체 히트맵
loser_heatmap <- draw_pitch() +
  stat_bin2d(data = loser_data, aes(x = x, y = y, fill = ..count..), bins = 8, alpha = 0.8) +
  scale_fill_viridis_c(name = "Pass Count") +
  labs(title = "Overall Loser Pass Heatmap", x = "Field Length", y = "Field Width") +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    legend.position = "right"
  )

# 두 개의 그래프를 나란히 출력
grid.arrange(winner_heatmap, loser_heatmap, ncol = 2)
