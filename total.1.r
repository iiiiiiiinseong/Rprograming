library(jsonlite)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggforce)
library(tidyr)

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
    # 왼쪽 코너
    geom_point(aes(x=0, y=0), color="black", size=3) +
    # 오른쪽 코너
    geom_point(aes(x=field_length, y=0), color="black", size=3) +
    # 오른쪽 상단 코너
    geom_point(aes(x=field_length, y=field_width), color="black", size=3) +
    # 왼쪽 상단 코너
    geom_point(aes(x=0, y=field_width), color="black", size=3) +
    # 네모 필드 테두리
    coord_fixed(ratio = 1) +
    theme_void()
}

# JSON 데이터를 기반으로 경기 통계를 추출하는 함수
extract_match_data <- function(events_data) {
  
  # 결과를 저장할 리스트 초기화
  teams <- list()
  
  # 각 이벤트 처리
  for (i in 1:nrow(events_data)) {
    event <- events_data[i, ]  # drop = TRUE로 벡터 형태로 가져오기
    
    # 'team' 항목에서 팀 이름 추출
    if (!is.null(event$team)) {
      team_name <- event$team$name
    }
    
    # 팀 통계 초기화 (팀 이름이 없으면 새로 만듦)
    if (!team_name %in% names(teams)) {
      teams[[team_name]] <- list(
        shots = 0,
        on_target = 0,
        fouls = 0,
        yellow_cards = 0,
        red_cards = 0,
        offsides = 0,
        corners = 0,
        passes = 0,
        pass_success = 0,
        pass_success_rate = 0
      )
    }
    
    # 이벤트 종류 추출
    event_type <- event$type$name
    
    # "Shot"인 경우
    if (event_type == "Shot") {
      teams[[team_name]]$shots <- teams[[team_name]]$shots + 1
      outcome <- event$shot$outcome$name
      if (outcome %in% c("Goal", "Saved", "Post")) {
        teams[[team_name]]$on_target <- teams[[team_name]]$on_target + 1
      }
    } 
    
    # "Pass"인 경우
    else if (event_type == "Pass") {
      teams[[team_name]]$passes <- teams[[team_name]]$passes + 1
      # 패스 성공 여부 확인
      if (is.null(event$pass$outcome)) {
        teams[[team_name]]$pass_success <- teams[[team_name]]$pass_success + 1
      } 
      
      # "Pass Offside" 확인
      outcome_name <- tryCatch(event$pass$outcome$name, error = function(e) NA)
      if (!is.na(outcome_name) && outcome_name == "Pass Offside") {
        teams[[team_name]]$offsides <- teams[[team_name]]$offsides + 1
      }
      
      # "Corner" 확인
      type_name <- tryCatch(event$pass$type$name, error = function(e) NA)
      if (!is.na(type_name) && type_name == "Corner") {
        teams[[team_name]]$corners <- teams[[team_name]]$corners + 1
      }
    }
    
    # "Foul Committed" 또는 "Bad Behaviour"인 경우
    else if (event_type %in% c("Foul Committed", "Bad Behaviour")) {
      teams[[team_name]]$fouls <- teams[[team_name]]$fouls + 1
      
      # 카드 정보 추출
      card_type1 <- if (!is.null(event$foul_committed$card)) {
        event$foul_committed$card$name
      } else {
        NA
      }
      
      card_type2 <- if (!is.null(event$bad_behaviour$card)) {
        event$bad_behaviour$card$name
      } else {
        NA
      }
      
      # Yellow Card 체크
      if (!is.na(card_type1) && card_type1 == "Yellow Card" || 
          !is.na(card_type2) && card_type2 == "Yellow Card") {
        teams[[team_name]]$yellow_cards <- teams[[team_name]]$yellow_cards + 1
      }
      
      # Red Card 체크
      if (!is.na(card_type1) && card_type1 %in% c("Red Card", "Second Yellow") || 
          !is.na(card_type2) && card_type2 %in% c("Red Card", "Second Yellow")) {
        teams[[team_name]]$red_cards <- teams[[team_name]]$red_cards + 1
      }
    } 
  }
  
  # 패스 성공률 계산
  for (team in names(teams)) {
    if (teams[[team]]$passes > 0) {
      teams[[team]]$pass_success_rate <- round((teams[[team]]$pass_success / teams[[team]]$passes) * 100, 2)
    }
  }
  
  return(list(teams = teams, team_names = unique(names(teams))))
}

# 결과 확인

# JSON 데이터를 기반으로 팀별 볼 점유율을 계산하는 함수
calculate_possession <- function(events_data) {
  
  team_possession <- list()
  total_duration <- 0
  
  for (i in 1:nrow(events_data)) {
    
    # 점유 팀과 지속 시간 가져오기 (내부 데이터 프레임 접근)
    possession_team <- events_data$possession_team$name[i]
    
    # duration 값이 NA일 경우 0으로 처리
    duration <- ifelse(!is.na(events_data$duration[i]), events_data$duration[i], 0)
    
    # 총 지속 시간 갱신
    total_duration <- total_duration + duration
    
    # 팀 점유 시간 갱신
    if (is.null(team_possession[[possession_team]])) {
      team_possession[[possession_team]] <- 0
    }
    # 지속 시간을 더해줌
    team_possession[[possession_team]] <- team_possession[[possession_team]] + duration
  }
  
  # 점유율 계산
  possession_percentages <- sapply(team_possession, function(time) (time / total_duration) * 100)
  
  return(list(possession_percentages = possession_percentages, total_duration_minutes = total_duration / 60))
}

# 경기 통계를 표로 출력하는 함수
create_match_table <- function(data, team_names, possession_data) {
  rows <- data.frame(
    Category = c("Shots", "On Target", "Possession", "Passes", "Pass Accuracy", "Foul", 
                 "Yellow Card", "Red Card", "Offside", "Corners"),
    Team1 = c(data[[team_names[1]]]$shots, data[[team_names[1]]]$on_target, 
              paste0(round(possession_data$possession_percentages[team_names[1]], 2), "%"), 
              data[[team_names[1]]]$passes, 
              paste0(data[[team_names[1]]]$pass_success_rate, "%"),
              data[[team_names[1]]]$fouls, data[[team_names[1]]]$yellow_cards, 
              data[[team_names[1]]]$red_cards, data[[team_names[1]]]$offsides, 
              data[[team_names[1]]]$corners),
    Team2 = c(data[[team_names[2]]]$shots, data[[team_names[2]]]$on_target, 
              paste0(round(possession_data$possession_percentages[team_names[2]], 2), "%"), 
              data[[team_names[2]]]$passes, 
              paste0(data[[team_names[2]]]$pass_success_rate, "%"),
              data[[team_names[2]]]$fouls, data[[team_names[2]]]$yellow_cards, 
              data[[team_names[2]]]$red_cards, data[[team_names[2]]]$offsides, 
              data[[team_names[2]]]$corners)
  )
  
  # 표 출력
  grid.table(rows)
}

# 패스 네트워크 그리기 함수
draw_pass_network <- function(events_data, lineup_data, side) {
  # 데이터 로드
  
  # 팀과 선발 명단 확인
  if (side == "home") {
    lineup_info <- lineup_data[1, ]
  } else if (side == "away") {
    lineup_info <- lineup_data[2, ]
  } else {
    stop("잘못된 사이드입니다. 'home' 또는 'away'를 선택하세요.")
  }
  
  team_name <- lineup_info$team_name
  lineup <- as.data.frame(lineup_info$lineup)
  
  starting_players <- c()
  
  # 선발 명단 (Starting XI) 추출
  for(i in 1:nrow(lineup)) {
    player <- lineup[i, ]
    
    start_reason <- player$positions[[1]]$start_reason
      
    # start_reason이 NULL이 아니고, "Starting XI"이 포함되어 있는지 확인 
    if("Starting XI" %in% start_reason) {
        starting_players <- c(starting_players, player$player_name)
    }
  }
  
  # 선택된 팀과 선수의 패스 필터링
  team_passes <- events_data %>%
    filter(type$name == "Pass") 
  
  # 각 선수의 패스 위치 수집
  player_positions <- team_passes %>%
    filter(player$name %in% starting_players) %>%
    mutate(
      # pass$recipient$name에 접근하여 NULL이 있는 경우 대체
      passer = player$name,
      start_x = sapply(location, function(x) x[[1]]),
      start_y = sapply(location, function(x) x[[2]]),
      recipient_name = ifelse(sapply(pass$recipient$name, is.null), "X", pass$recipient$name),
      end_x = sapply(pass$end_location, function(x) x[[1]]),
      end_y = sapply(pass$end_location, function(x) x[[2]])
    ) %>%
    select(passer, start_x, start_y, recipient_name, end_x, end_y)
  
  # 패스 카운트 계산
  pass_counter <- team_passes %>%
    mutate(
      passer = player$name,
      # pass$recipient$name에 접근하여 NULL이 있는 경우 대체
      recipient_name = ifelse(sapply(pass$recipient$name, is.null), "X", pass$recipient$name)
    ) %>%
    filter(passer %in% starting_players, recipient_name %in% starting_players) %>%
    count(passer, recipient_name)
  
  # 선수별 평균 위치 계산
  average_positions <- player_positions %>%
    group_by(passer) %>%
    summarise(
      avg_x = mean(c(start_x, end_x), na.rm = TRUE),
      avg_y = mean(c(start_y, end_y), na.rm = TRUE)
    )
  
  # 각 선수의 총 패스 수 계산 (보낸 패스 + 받은 패스)
  pass_counts_sent <- pass_counter %>%
    group_by(passer) %>%
    summarise(sent = sum(n))
  
  pass_counts_received <- pass_counter %>%
    group_by(recipient_name) %>%
    summarise(received = sum(n))
  
  pass_counts <- full_join(pass_counts_sent, pass_counts_received, 
                           by = c("passer" = "recipient_name")) %>%
    replace_na(list(sent = 0, received = 0)) %>%
    mutate(total = sent + received) %>%
    rename(player = passer)
  
  # 축구 필드 그리기
  field_plot <- draw_pitch()
  
  # 패스 연결선 그리기
  pass_lines <- pass_counter %>%
    left_join(average_positions, by = c("passer" = "passer")) %>%
    rename(passer_x = avg_x, passer_y = avg_y, count = n) %>%
    left_join(average_positions, by = c("recipient_name" = "passer")) %>%
    rename(recipient_x = avg_x, recipient_y = avg_y)
  
  # 필드에 패스 연결선 추가
  field_plot <- field_plot +
    geom_segment(data = pass_lines, 
                 aes(x = passer_x, y = passer_y, xend = recipient_x, yend = recipient_y, 
                     linewidth = count),  
                 color = "blue", alpha = 0.5) +
    scale_size(range = c(0.5, 2), name = "패스 수")
  
  pass_counts <- pass_counts %>%
    left_join(average_positions, by = c("player" = "passer"))
  
  # 선수 위치와 이름 표시
  field_plot <- field_plot +
    # 선수 위치 표시
    geom_point(data = average_positions, aes(x = avg_x, y = avg_y), 
               color = "red", size = 3, stroke = 1, shape = 21, fill = "yellow") +
    
    # 선수 이름 표시
    geom_text(data = average_positions, aes(x = avg_x, y = avg_y + 2, label = passer), 
              size = 3, hjust = 0.5, vjust = 0) +
    
    # 패스 수에 따른 노드 크기 조절
    geom_point(data = pass_counts, 
               aes(x = avg_x, y = avg_y, size = total),  # match 사용하지 않음
               color = "black", alpha = 0.5) +
    scale_size_continuous(range = c(3, 10), name = "총 패스 수") 
  
  # 최종 그래프 설정
  field_plot <- field_plot +
    ggtitle(paste(team_name, "Pass Network (Starting Xi)")) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.position = "bottom"
    )
  
  # 플롯 출력
  print(field_plot)
}

# shot map
draw_shot_map <- function(events_data, lineup_data, side = "home") {
  # 데이터 로드
  
  # 팀과 선발 명단 확인
  if (side == "home") {
    lineup_info <- lineup_data[1, ]
  } else if (side == "away") {
    lineup_info <- lineup_data[2, ]
  } else {
    stop("Invalid side. Choose 'home' or 'away'.")
  }
  
  side_team <- lineup_info$team_name
  
  # 슛 위치, 결과, 카테고리 추출
  team_shots <- events_data %>%
    filter(type$name == "Shot" & team$name == side_team)
  
  shot_locations <- team_shots$location
  shot_outcomes <- team_shots$shot$outcome$name
  
  shot_categories <- sapply(shot_outcomes, function(outcome) {
    if (outcome == "Goal") {
      return("Goal")
    } else if (outcome %in% c("Saved", "Post")) {
      return("On Target")
    } else {
      return("Off Target")
    }
  })
  
  # plot 위해 데이터프레임 생성
  shots_df <- data.frame(
    x = sapply(shot_locations, function(loc) loc[1]),
    y = sapply(shot_locations, function(loc) loc[2]),
    category = shot_categories
  )
  
  # 축구 필드
  field_plot <- draw_pitch()
  
  # 슛 카테고리 별로 색 지정
  category_colors <- c("Goal" = "green", "On Target" = "blue", "Off Target" = "red")
  
  # 슛 표시
  shot_plot <- field_plot +
    geom_point(data = shots_df, aes(x = x, y = y, color = category), 
               size = 4, alpha = 0.7) +
    scale_color_manual(values = category_colors, name = "Shot Outcome") +
    ggtitle(paste(side, "Team Shot Map:", side_team)) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.position = "bottom"
    )
  
  print(shot_plot)
}

# 파일 경로
events_file_path <- "C:/Users/user/local/GitHub/open-data/data/events/3773457.json"
lineup_file_path <- "C:/Users/user/local/GitHub/open-data/data/lineups/3773457.json"

events_data <- fromJSON(events_file_path)
lineup_data <- fromJSON(lineup_file_path)

# 데이터 추출
match_data <- extract_match_data(events_data)
team_names <- match_data$team_names

# 점유율 계산
possession_data <- calculate_possession(events_data)

# 표 생성 및 출력
create_match_table(match_data$teams, team_names, possession_data)

draw_pass_network(events_data, lineup_data , side = "home")
draw_pass_network(events_data, lineup_data , side = "away")

draw_shot_map(events_data, lineup_data, side = "home")
draw_shot_map(events_data, lineup_data, side = "away")