import json
import pandas as pd

# JSON 파일 로드
with open('C:/Users/user/local/GitHub/open-data/data/matches/11/1.json', 'r', encoding='utf-8') as f1:
    data = json.load(f1)

# DataFrame 변환
df = pd.DataFrame(data)

# 컬럼 이름 확인
print(df.columns)

# 원하는 항목만 추출
df['home_team_name'] = df['home_team'].apply(lambda x: x['home_team_name'])
df['away_team_name'] = df['away_team'].apply(lambda x: x['away_team_name'])
df['competition_name'] = df['competition'].apply(lambda x: x['competition_name'])
df['season_name'] = df['season'].apply(lambda x: x['season_name'])

columns_to_display = ['match_id',
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

# 출력
print(df_selected.head())

# CSV 파일 저장
output_path = 'C:/CODING/R/R_TeamProject/matches_11_1/1.csv'
df_selected.to_csv(output_path, index=False, encoding='utf-8-sig')

print(f"CSV 파일이 {output_path}에 저장되었습니다.")


