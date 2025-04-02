library(shiny)
library(leaflet)
library(jsonlite)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      html, body {
        height: 100%;
        margin: 0;
        padding: 0;
      }
      #map-container {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 100vh;
        width: 100vw;
      }
      #map { 
        height: 90vh;   /* 使用视窗高度的90% */
        width: 90vw;    /* 使用视窗宽度的90% */
        margin: auto;   /* 自动外边距确保居中 */
      }
      .tree-form { padding: 10px; }
      .tree-form input { margin: 5px 0; width: 100%; }
      .tree-info { padding: 10px; }
      .control-panel { 
        position: absolute; 
        top: 10px; 
        left: 10px; 
        background: white; 
        padding: 10px; 
        z-index: 1000; 
        border: 1px solid #ccc; 
        border-radius: 4px;
      }
      .control-panel button, .control-panel input { margin-top: 5px; }
    "))
  ),
  
  div(id = "map-container",
      leafletOutput("map")
  ),
  
  absolutePanel(
    class = "control-panel",
    top = 10, left = 10,
    div("点击地图添加树并编辑资料"),
    actionButton("export", "导出树数据"),
    fileInput("loadFile", "加载树数据", accept = ".json")
  ),
  
  tags$script(HTML("
    var markers = {};
    var markerIdCounter = 0;
    var map; // 全局存储地图对象
    
    var greenDotIcon = L.divIcon({
      className: 'custom-dot',
      html: '<div style=\"background-color: green; width: 8px; height: 8px; border-radius: 50%;\"></div>',
      iconSize: [8, 8],
      iconAnchor: [4, 4]
    });
    
    function getTreeInfoContent(marker) {
      return `
        <div class=\"tree-info\">
          <b>树信息</b><br>
          树种: ${marker.treeData.species || '未指定'}<br>
          坐标: ${marker.treeData.lat}, ${marker.treeData.lng}<br>
          <button onclick=\"Shiny.setInputValue('edit_tree', '${marker.id}', {priority: 'event'})\">编辑</button>
          <button onclick=\"Shiny.setInputValue('delete_tree', '${marker.id}', {priority: 'event'})\">删除</button>
        </div>
      `;
    }
    
    // 确保地图对象可用后再处理数据
    function safeAddMarkers(treeDataArray) {
      if (!map) {
        console.error('地图未初始化，等待重试...');
        setTimeout(function() { safeAddMarkers(treeDataArray) }, 100);
        return;
      }
      
      treeDataArray.forEach(function(treeData) {
        try {
          var lat = parseFloat(treeData.lat);
          var lng = parseFloat(treeData.lng);
          if (isNaN(lat) || isNaN(lng)) {
            console.error('无效坐标:', treeData);
            return;
          }
          
          var marker = L.marker([lat, lng], { icon: greenDotIcon }).addTo(map);
          marker.id = 'marker-' + markerIdCounter++;
          marker.treeData = {
            species: treeData.species || '',
            lat: lat.toFixed(6),
            lng: lng.toFixed(6)
          };
          markers[marker.id] = marker;
          marker.bindPopup(getTreeInfoContent(marker));
          console.log('添加标记:', marker.id);
        } catch (error) {
          console.error('添加标记失败:', error);
        }
      });
    }
    
    Shiny.addCustomMessageHandler('loadTreeData', function(treeDataArray) {
      console.log('收到数据，类型:', Array.isArray(treeDataArray) ? '数组' : typeof treeDataArray);
      safeAddMarkers(treeDataArray);
    });
  "))
)

server <- function(input, output, session) {
  # 初始化地图并暴露 map 对象到全局
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 113.2943, lat = 23.0965, zoom = 17) %>%  # 调整zoom级别为17
      htmlwidgets::onRender("
        function(el, x) {
          // 将地图对象赋值给全局变量
          window.map = this;
          console.log('地图初始化完成');
        }
      ")
  })
  
  # 自动加载树数据
  observe({
    if (file.exists("treedata.json")) {
      tryCatch({
        treeData <- fromJSON("treedata.json", simplifyVector = FALSE)
        cat("数据加载成功，条目数:", length(treeData), "\n")
        session$sendCustomMessage(type = "loadTreeData", treeData)
      }, error = function(e) {
        cat("加载失败:", e$message, "\n")
      })
    }
  })
  
  # 文件上传处理
  observeEvent(input$loadFile, {
    req(input$loadFile)
    tryCatch({
      treeData <- fromJSON(input$loadFile$datapath, simplifyVector = FALSE)
      session$sendCustomMessage(type = "loadTreeData", treeData)
    }, error = function(e) {
      showNotification(paste("加载失败:", e$message), type = "error")
    })
  })
  
  # 其他功能保持不变
  observeEvent(input$save_tree, {
    session$sendCustomMessage(type = "updateMarker", list(
      id = input$save_tree$id,
      species = input$save_tree$species
    ))
  })
  
  output$export <- downloadHandler(
    filename = "tree_data.json",
    content = function(file) {
      shinyjs::runjs("
        var treeDataArray = Object.values(markers).map(marker => marker.treeData);
        Shiny.setInputValue('exported_data', treeDataArray, {priority: 'event'});
      ")
      req(input$exported_data)
      write(toJSON(input$exported_data, auto_unbox = TRUE, pretty = TRUE), file)
    }
  )
}

shinyApp(ui, server)