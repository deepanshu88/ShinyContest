mystyle <- function() {
  tags$head(
    tags$style(
      HTML("

  .main-header .sidebar-toggle {padding: 19px 15px !important;}
  .main-header .logo {height: 60px !important; padding-left: 25px !important;}
  .main-header .navbar-custom-menu, .main-header .navbar-right {margin-top: 10px;}

.tile-stats .count {
    font-family: 'Bangers';
    font-size: 48px !important;
    font-weight: 400 !important;
    color : #dd4b39;
}

.tile-stats h3 {
    font-weight: 300 !important;
    font-size: 24px !important;
    color : #555 !important;
}


.sidebar .fa, .fas {
    color: #ddd !important;
}

.sidebar span {
    color: #f4f4f4 !important;
}

.left-side, .main-sidebar, .wrapper {
    background-color: #333 !important;
}

.nav>li>a:active, .nav>li>a:focus, .nav>li>a:hover {
    color: #fff;
    background: none;
}

         .text-uppercase2 {
            text-transform: uppercase!important;
            font-weight: 600;
            color: #444;
            font-size: 16px;
          }

         body {color:#333;font-family: 'Helvetica Neue', Roboto, Arial, 'Droid Sans', sans-serif}
        .box {border-top: none;}
        .box>.box-body{padding:0; border:none;}
        
        .description-block{text-align:left;padding-left: 20px;
         padding-bottom:10px; padding-right:10px; padding-top:0px; position:relative;
         text-overflow:ellipsis; white-space:nowrap;}
        
        .description-block>.description-text {text-transform: none; color:#73879C;font-size: 16px;}
        .description-block>.description-header {font-size: 30px; color:#73879C; line-height: 1.5;}
        
        .box .border-right {border-right: 1px solid #ADB2B5}
        
        .text-align {
          font-size: 16px;
          margin-left: 10px;
          }
        
         h5.card-title2{font-size:16px;}
        
        .mycss0 {font-size: 13px; color:#666;text-overflow: ellipsis; overflow: hidden;
                white-space: nowrap; position: relative;}

        .mycss {font-size: 13px; color:#73879C;text-overflow: ellipsis; overflow: hidden;
                white-space: nowrap; position: relative;}
                
        .mycss2 {font-size: 16px; margin-top:5px; color: #666;}
        
        .mydescription-text {text-transform: none; color:#73879C;font-size: 17px;}
        .mydescription-header {font-size: 20px; color:#73879C; font-weight:bold;}   
        .mydescription-header2 {font-size: 18px; color:#444; font-weight:bold;}  
        
        #mybox .box-footer{background-color: rgb(236, 240, 245);}
        #mybox .pad{padding-top:0px; margin-top: -10px;}
        #mybox .col-sm-2 {padding-right:0px;}

        .sidebar-mini.sidebar-collapse .content-wrapper, .sidebar-mini.sidebar-collapse .main-footer, .sidebar-mini.sidebar-collapse .right-side {
          margin-left: 65px!important;}


.sidebar-mini.sidebar-collapse .sidebar span {
    font-size: 20px;
    font-family: 'Racing Sans One';
    color: #666;
    display: block;
    font-weight: normal;
    text-align: center;
    width: 100% !important;
}


.sidebar-mini.sidebar-collapse .sidebar .fa, .fas {
    font-weight: 900;
    text-align: center;
    font-size: 36px;
    width: 100% !important;
    margin-bottom: 10px;
    color: #444;
}

.sidebar-collapse .main-header .logo {
    width: 90px !important; 
    padding-top:10px;
}

.sidebar-collapse .main-header .logo .logo-mini {
  font-size: 30px !important;
}


.sidebar-collapse .main-header .navbar .sidebar-toggle {
    color: #fff;
    padding-left: 30px;
}

.main-header .logo {
    background-color: #f4f4f4;
}
        
.sidebar-collapse .sidebar-menu>li>a {
    font-weight: 600;
    padding-right: 20px;
    text-align: center !important;
}

.sidebar-mini.sidebar-collapse .main-header .navbar {
    margin-left: 90px;
}

.sidebar-mini.sidebar-collapse .main-sidebar {
    width: 90px!important;
    margin-top: 20px;
}



.sidebar a {
    color: #FFF;
}

.sidebar-collapse .content {
    padding-left: 45px;
 }

.sidebar-collapse .sidebar-menu {
    text-align: center !important;
}

   .myClass { 
        font-size: 28px;
        font-weight:bold;
        line-height: 60px;
        text-align: left;
        padding: 0 15px;
        overflow: hidden;
        color: white;
    }
    
    .main-color {color: #ffc107}
    
                
        ")
    ),
    #### ---- JS ----####
    tags$script(HTML(
      '$(document).ready(function() {
        $("header").find("nav").append(\'<span id="pageHeader" class="myClass"></span>\');
      })')),
    tags$link(rel="stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "init.js")
  )
}
