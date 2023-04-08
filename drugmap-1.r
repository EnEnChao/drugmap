# 載入需要的套件：
install.packages(c("sf", "dplyr", "leaflet", "classInt", "tidyverse", "sf", "htmlwidgets"))
library(sf)
library(dplyr)
library(leaflet)
library(classInt)
library(tidyverse)
library(sf)
library(htmlwidgets)

# 製作虛擬資料：
# 設定隨機種子
set.seed(123)

# 創建虛擬資料
data_generated <- tibble(
  year = sample(2010:2020, 200 * 12 * 11, replace = TRUE),
  month = sample(1:12, 200 * 12 * 11, replace = TRUE),
  district = sample(c("桃園區", "中壢區", "八德區", "龜山區", "平鎮區",
                      "大溪區", "楊梅區", "蘆竹區", "大園區", "龍潭區",
                      "新屋區", "觀音區", "復興區"), 200 * 12 * 11, replace = TRUE,
                   prob = c(0.19, 0.17, 0.14, 0.12, 0.11, 0.06, 0.05, 0.04, 0.03, 0.03, 0.02, 0.02, 0.02)),
  count = sample(1:5, 200 * 12 * 11, replace = TRUE),
  lon = runif(200 * 12 * 11, 121.0, 121.5),
  lat = runif(200 * 12 * 11, 24.5, 25.0)
)

# 計算各行政區的總案件數
data_summary <- data_generated %>%
  group_by(district) %>%
  summarise(total_count = sum(count))

# 讀取桃園市行政區的地理資料：
districts_data <- st_read("taoyuan_districts.geojson") %>% st_transform(crs = 4326)

# 連接地理資料和統計資料：
districts_joined <- left_join(districts_data, data_summary, by = c("T_Name" = "district"))

# 定義顏色函數
n_classes <- 5
class_intervals <- classIntervals(districts_joined$total_count, n = n_classes, style = "quantile")
pal <- colorBin("YlOrRd", domain = districts_joined$total_count, bins = class_intervals$brks, pretty = TRUE)

# 創建互動式熱區地圖
m <- leaflet(districts_joined) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(total_count),
    fillOpacity = 0.8,
    color = "white",
    weight = 1,
    label = ~paste0("行政區: ", T_Name, "/案件數: ", total_count),
    labelOptions = labelOptions(
      style = list(
        "background-color" = "darkblue",
        "color" = "white",
        "border-color" = "white",
        "font-size" = "14px"
      ),
      textsize = "14px",
      direction = "auto"
    )
  ) %>%
  addLegend(pal = pal, values = ~total_count, opacity = 0.8, title = "案件數量")

# 印出地圖
m

