# Statement ----
# 用于分析目前所有菜谱的调味料需求。长期目标是回答一个问题：新搬家到一个地方，如果购买调味料，才能获得最满意的烹饪体验？
# 漏洞：如何判断可相互代替的食材或调料，比如“没有冰糖的话，可用砂糖代替”，如果不结合语义分析，该文本的判断结果就会变成这个菜谱既需要冰糖，又需要砂糖。

# Package ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)

# Setting ----
showtext_auto()

# Function ----

# Read data ----
# 菜谱名称列表
recipe.list <- list.files("RawData") %>% 
  # 仅保留*.md文件
  # 漏洞：有些原始文件包含附件，导出为文件夹形式，应该如何处理呢？
  .[grepl("md", .)]

# 读取所有菜谱
recipe <- vector("list", length(recipe.list))
names(recipe) <- recipe.list
for (i in recipe.list) {
  recipe[[i]] <- readLines(paste0("RawData/", i)) %>% 
    paste0(collapse = "/n")
}

# 调味料列表
# 调味料分类来自：https://zh.wikipedia.org/zh-cn/%E8%B0%83%E5%91%B3%E6%96%99#%E8%AA%BF%E5%91%B3%E6%96%99%E5%88%97%E8%A1%A8
condiment.list <- c(
  # 单一成分
  "盐", "糖", 
  # 单一植物或真菌
  # 漏洞：广义上“香油”可以指代多种植物调味油，“罗勒”和“九层塔”是同义词
  "胡椒粉", "麻油", "香油", "罗勒", "九层塔", "香菜", "孜然", "百里香", 
  "迷迭香", "草果", "香叶", "八角", 
  # 酒醋
  "米酒", "红酒", "味淋", "料酒", "香醋", "老醋", "陈醋", "白酒", "白葡萄酒", 
  # 动物来源
  "鱼露", "干贝", 
  # 酱类
  "酱油", "生抽", "老抽", "豆豉", "腐乳", "豆瓣酱", "甜面酱", "蚝油", 
  # 混合粉类
  "咖喱"
)

# Analysis -----
# 提取每份菜谱的名称
req.condiment <- tibble(menu = str_split_fixed(recipe.list, " ", n = 2)[, 1])

# 提取每份菜谱所含的调味料
for (i in condiment.list) {
  req.condiment[[i]] <- grepl(i, recipe)
}

# 查看最常用的调味料
req.condiment %>% 
  pivot_longer(cols = -menu, names_to = "condiment") %>% 
  group_by(condiment) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  ggplot() + 
  geom_col(aes(reorder(condiment, value), value)) + 
  coord_flip()

# 筛选出仅含有基础调味料的菜谱
tar.condiment <- c("盐", "糖", "胡椒粉", "生抽", "酱油", "料酒", "鱼露", "蚝油")
non.tar.condiment <- setdiff(condiment.list, tar.condiment)
req.condiment %>% 
  pivot_longer(cols = -menu, names_to = "condiment") %>% 
  subset(value == TRUE & condiment %in% non.tar.condiment) %>% 
  select(menu) %>% 
  distinct() %>% 
  setdiff(select(req.condiment, menu), .) %>% 
  View()
