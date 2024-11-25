import json
import pandas as pd

# JSON 파일 로드
with open('C:/Users/user/local/GitHub/open-data/data/competitions.json', 'r') as f1:
    data = json.load(f1)

# DataFrame 변환
df = pd.DataFrame(data)

# 컬럼 이름 확인
print(df.columns)

columns_to_display = ['competition_id',
                      'season_id',
                      'country_name',
                      'competition_name',
                      'season_name',
                      'match_updated',
                      'match_available'
                      ]
df_selected = df[columns_to_display]

# 출력
print(df_selected.head())

# CSV 파일 저장
output_path = 'C:/CODING/R/R_TeamProject/competitons/competitions.csv'
df_selected.to_csv(output_path, index=False)


