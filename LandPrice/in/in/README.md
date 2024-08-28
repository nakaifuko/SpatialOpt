# How to make Toyo_SocioEconomicData.csv
## Data
 - All the data below is available in the database provided by MLIT https://nlftp.mlit.go.jp/

### 緊急輸送道路 emergency transport road
 - If the grid intersects with an emergency transport road, set 'emRoad' to 1.

### 都市計画区域 Urban Planning Zoning
 - If the grid is '市街化区域', set 'Shigaika' to 2.
 - If the grid is '市街化調整区域', set 'Chosei' to 3.

### 鉄道 Distance from Railway station
 - 'distance' represents the istance [m] from railway station.

### 地価公示データ Land Price
 - L01_100: 地価公示データ Chika-koji (Land Market Value Publication) 
 - L02_094: 都道府県地価調査データ Chika-chosa (Prefectural Land Price Survey)
 - LandPrice: L01_100 or L02_094
