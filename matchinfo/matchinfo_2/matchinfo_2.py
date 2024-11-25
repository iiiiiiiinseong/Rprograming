import os
import json
import pandas as pd

# JSON 파일이 위치한 폴더 경로
folder_path = 'C:/Users/user/local/GitHub/open-data/data/matches/2'

# 데이터를 저장할 리스트 초기화
data_list = []

# 폴더 내 JSON 파일 순회
for file_name in os.listdir(folder_path):
    if file_name.endswith('.json'):  # JSON 파일만 처리
        file_path = os.path.join(folder_path, file_name)

        # JSON 파일 읽기
        with open(file_path, 'r', encoding='utf-8') as file:
            data = json.load(file)

            # JSON 데이터가 리스트 형태일 경우 첫 번째 항목 선택
            match_data = data[0]

            # 필요한 정보 추출
            competition_name = match_data['competition']['competition_name']
            season = match_data['season']['season_name']
            match_date = match_data['match_date']
            kick_off = match_data['kick_off']
            stadium = match_data['stadium']['name']
            home_team = match_data['home_team']['home_team_name']
            away_team = match_data['away_team']['away_team_name']

            # 데이터를 딕셔너리 형태로 저장
            data_list.append({
                "Competition": competition_name,
                "Season": season,
                "Match Date": match_date,
                "Kick Off": kick_off,
                "Stadium": stadium,
                "Home Team": home_team,
                "Away Team": away_team
            })

# 데이터프레임 생성
df = pd.DataFrame(data_list)

# 데이터프레임 출력
print(df)
