# 简要说明

将原本基于无穷JSON的系统迁移至RShiny实现

地址：https://sayabao.shinyapps.io/treemap/

当前功能包括：

1. **数据读取**
   - 自动读取工作目录下的`treedata.json`文件
   - 支持用户自定义导入JSON文件

2. **地图标注**
   - 无穷原本的json里有这个功能，但是转成rshiny的时候有问题，现在没法打点，导出功能也未测试

3. **数据格式要求**
```json
[
    {
      "species": "榕树",
      "lat": "23.096500",
      "lng": "113.294300"
    },
    {
      "species": "木棉",
      "lat": "23.096600",
      "lng": "113.294400"
    }
]
