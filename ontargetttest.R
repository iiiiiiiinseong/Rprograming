library(jsonlite)
library(dplyr)
library(ggplot2)

# 1. 'C:/CODING/R/R_TeamProject/Laliga_10_21'에 있는 여러 JSON 파일에서 match_id 추출
folder_path_laliga <- 'C:/CODING/R/R_TeamProject/Laliga_10_21'
json_files_laliga <- list.files(folder_path_laliga, pattern = "*.json", full.names = TRUE)

# match_id를 담을 리스트
match_ids <- c()

# 각 JSON 파일에서 match_id 추출
for (json_file in json_files_laliga) {
  data <- fromJSON(json_file)
  match_ids <- c(match_ids, data[["match_id"]])
}

# 2. match_id를 이용해 경기 이벤트 데이터 로드
folder_path_events <- 'C:/Users/user/local/GitHub/open-data/data/events'
event_data <- list()

for (match_id in match_ids) {
  event_file_path <- file.path(folder_path_events, paste0(match_id, ".json"))
  
  if (file.exists(event_file_path)) {
    event_data[[as.character(match_id)]] <- fromJSON(event_file_path)
  } else {
    cat(paste0("Warning: ", match_id, "에 대한 경기 파일이 존재하지 않습니다.\n"))
  }
}

# 3. 각 경기에서 승리팀과 패배팀 추출
winning_shots <- c()
losing_shots <- c()

for (match in event_data) {
  # 홈팀과 어웨이팀 정보 추출
  home_team <- match$team$name[1]
  away_team <- match$team$name[2]


  # 각 팀의 득점 수 계산 (shot.outcome.name에서 Goal 찾기)
  home_goals <- sum(sapply(match, function(event) {
    if (!is.null(event$type$name) && event$type$name == "Shot" && event$shot$outcome$name == 'Goal' && event$possession_team$name == home_team) {
      return(1)
    }
    return(0)
  }))
  
  away_goals <- sum(sapply(match, function(event) {
    if (!is.null(event$type$name) && event$type$name == "Shot" && event$shot$outcome$name == 'Goal' && event$possession_team$name == away_team) {
      return(1)
    }
    return(0)
  }))
  
  # 승리팀과 패배팀 판별
  if (home_goals > away_goals) {
    winning_team <- home_team
    losing_team <- away_team
  } else {
    winning_team <- away_team
    losing_team <- home_team
  }
  
  # 유효슛 계산 (goal, post, saved만 유효슛으로 처리)
  winning_shots_count <- 0
  losing_shots_count <- 0
  
  for (event in match) {
    if (event$type$name == 'Shot') {  # Shot 이벤트만 처리
      if (!is.null(event$shot) && !is.null(event$shot$outcome)) {
        outcome <- event$shot$outcome$name
        if (outcome %in% c('Goal', 'Saved', 'Post')) {
          if (event$possession_team$name == winning_team) {
            winning_shots_count <- winning_shots_count + 1
          } else if (event$possession_team$name == losing_team) {
            losing_shots_count <- losing_shots_count + 1
          }
        }
      }
    }
  }
  
  winning_shots <- c(winning_shots, winning_shots_count)
  losing_shots <- c(losing_shots, losing_shots_count)
}

# 4. T-test 진행
df <- data.frame(Winning_Team_Shots = winning_shots, Losing_Team_Shots = losing_shots)

t_test_result <- t.test(df$Winning_Team_Shots, df$Losing_Team_Shots)
cat("t-statistic: ", t_test_result$statistic, "\n")
cat("p-value: ", t_test_result$p.value, "\n")

if (t_test_result$p.value < 0.05) {
  cat("유효슛과 경기 승패 간에 유의미한 차이가 존재합니다.\n")
} else {
  cat("유효슛과 경기 승패 간에 유의미한 차이가 존재하지 않습니다.\n")
}

# 5. 정규분포 그래프
ggplot() +
  geom_density(aes(x = winning_shots), fill = 'blue', alpha = 0.5) +
  geom_density(aes(x = losing_shots), fill = 'red', alpha = 0.5) +
  labs(title = 'Distribution of Shots: Winning vs Losing Teams',
       x = 'Number of On Target Shots',
       y = 'Density') +
  theme_minimal() +
  scale_color_manual(values = c('blue', 'red')) +
  theme(legend.position = 'top') +
  theme(plot.title = element_text(hjust = 0.5))
