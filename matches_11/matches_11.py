import os
import json
import pandas as pd

# JSON 파일이 있는 폴더 경로 설정
input_folder = 'C:/Users/user/local/GitHub/open-data/data/events/matches/11/'
output_path = 'C:/CODING/R/R_TeamProject/matches_11/combined.csv'

# 결과를 저장할 리스트
all_data = []

# 폴더 내 모든 JSON 파일 처리
for filename in os.listdir(input_folder):
    if filename.endswith('.json'):  # JSON 파일만 처리
        file_path = os.path.join(input_folder, filename)
        with open(file_path, 'r', encoding='utf-8') as f:
            data = json.load(f)
            df = pd.DataFrame(data)

            # 원하는 데이터만 추출
            df['home_team_name'] = df['home_team'].apply(lambda x: x['home_team_name'])
            df['away_team_name'] = df['away_team'].apply(lambda x: x['away_team_name'])
            df['competition_name'] = df['competition'].apply(lambda x: x['competition_name'])
            df['season_name'] = df['season'].apply(lambda x: x['season_name'])

            columns_to_display = [
                'match_id',
                'match_date',
                'kick_off',
                'competition_name',
                'season_name',
                'home_team_name',
                'away_team_name',
                'home_score',
                'away_score',
                'match_status',
                'stadium'
            ]
            df_selected = df[columns_to_display]

            # 각 파일의 데이터 추가
            all_data.append(df_selected)

# 모든 데이터를 하나의 DataFrame으로 병합
final_df = pd.concat(all_data, ignore_index=True)

# CSV 파일로 저장
final_df.to_csv(output_path, index=False, encoding='utf-8-sig')
print(f"모든 JSON 파일을 병합한 CSV 파일이 {output_path}에 저장되었습니다.")
