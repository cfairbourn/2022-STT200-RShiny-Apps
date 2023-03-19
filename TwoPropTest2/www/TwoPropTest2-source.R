# HTML code for editable table
html_table <- HTML(
  "<script type='text/javascript'>
  /*<![CDATA[*/
  function Expand1(){
  if (!R1N.savesize) R1N.savesize=R1N.size;
  if (!R2N.savesize) R2N.savesize=R2N.size;
  var isChrome = !!window.chrome && (!!window.chrome.webstore || !!window.chrome.runtime);
  var isSafari = /constructor/i.test(window.HTMLElement) || (function (p) { 
  return p.toString() === '[object SafariRemoteNotification]'; })(!window['safari'] || (typeof safari !== 'undefined' && safari.pushNotification));
  var isIE = /*@cc_on!@*/false || !!document.documentMode;
  offset = 0;
  if (isSafari) offset = 3;
  R1N.size=Math.max(R1N.savesize,R1N.value.length,R2N.value.length)-offset;
  R2N.size=Math.max(R2N.savesize,R1N.value.length,R2N.value.length)-offset;
  }
  function Expand2(){
  if (!C1N.savesize) C1N.savesize=C1N.size;
  if (!TL.savesize) TL.savesize=TL.size;
  if (!BL.savesize) BL.savesize=BL.size;
  var isChrome = !!window.chrome && (!!window.chrome.webstore || !!window.chrome.runtime);
  var isSafari = /constructor/i.test(window.HTMLElement) || (function (p) { return p.toString() === '[object SafariRemoteNotification]'; })(!window['safari'] || (typeof safari !== 'undefined' && safari.pushNotification));
  var isIE = /*@cc_on!@*/false || !!document.documentMode;
  offset = 0;
  if (isSafari) offset = 3;
  C1N.size=Math.max(C1N.savesize,C1N.value.length,TL.value.length,BL.value.length)-offset;
  TL.size=Math.max(TL.savesize,C1N.value.length,TL.value.length,BL.value.length)-offset;
  BL.size=Math.max(BL.savesize,C1N.value.length,TL.value.length,BL.value.length)-offset;
  }
  function Expand3(){
  if (!C2N.savesize) C2N.savesize=C2N.size;
  if (!TR.savesize) TR.savesize=TR.size;
  if (!BR.savesize) BR.savesize=BR.size;
  var isChrome = !!window.chrome && (!!window.chrome.webstore || !!window.chrome.runtime);
  var isSafari = /constructor/i.test(window.HTMLElement) || (function (p) { return p.toString() === '[object SafariRemoteNotification]'; })(!window['safari'] || (typeof safari !== 'undefined' && safari.pushNotification));
  var isIE = /*@cc_on!@*/false || !!document.documentMode;
  offset = 0;
  if (isSafari) offset = 3;
  C2N.size=Math.max(C2N.savesize,C2N.value.length,TR.value.length,BR.value.length)-offset;
  TR.size=Math.max(TR.savesize,C2N.value.length,TR.value.length,BR.value.length)-offset;
  BR.size=Math.max(BR.savesize,C2N.value.length,TR.value.length,BR.value.length)-offset;
  }
  /*]]>*/
  </script><style>
  table{
  border-color: #f3f7fb;
  display: block;
  overflow-x: auto;
  }
  
  th, td {
  border: 1px solid black;
  border-collapse: collapse;
  padding: 6px;
  }
  input {
  border: 0;
  width: auto;
  padding: 1px 8px;
  background-color: #f5f5f5;
  font-size: 1em;
  }
  
  
  </style>
  <table id = 'mytable'>
  <tbody>
  <tr>
  <td></td>
  <td><input size='10' id='C1N'type = 'text' onchange='Expand2();' oninput='Expand2();' value='Column A'></td>
  <td><input size='10' id='C2N'type = 'text' onchange='Expand3();' oninput='Expand3();' value='Column B'></td>
  <td><div style='padding: 1px 8px'>Total</div></td>
  </tr>
  <tr>
  <td><input size='6' id='R1N'type = 'text' onchange='Expand1();' oninput='Expand1();' value='Row 1'></td>
  <td><input size='1' id='TL' type='text' onchange='Expand2();' oninput='Expand2();' value='0'></td>
  <td><input size='1' id='TR' type = 'text' onchange='Expand3();' oninput='Expand3();' value='0'></td>
  <td><div size='1' style='padding: 1px 8px' id='TRT' class='shiny-text-output'></div></td>
  </tr>
  <tr>
  <td><input size='6' id='R2N' type = 'text' onchange='Expand1();' oninput='Expand1();' value='Row 2'></td>
  <td><input size='1' id='BL' type = 'text' onchange='Expand2();' oninput='Expand2();' value='0'></td>
  <td><input size='1' id='BR' type = 'text' onchange='Expand3();' oninput='Expand3();' value='0'></td>
  <td><div size='1' style='padding: 1px 8px' id='TRB' class='shiny-text-output'></div></td>
  </tr>
  <tr>
  <td><div style='padding: 1px 8px'>Total</div></td>
  <td><div size='1' style='padding: 1px 8px' id='TBL' class='shiny-text-output'></div></td>
  <td><div size='1' style='padding: 1px 8px' id='TBR' class='shiny-text-output'></div></td>
  <td><div size='1' style='padding: 1px 8px' id='Total' class='shiny-text-output'></div></td>
  </tr>
  </tbody>
  </table>")


#theme for plots
plaintheme <- theme_bw() + 
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() ) +
  theme(axis.line.x = element_line(color="black", size = 1),
        axis.line.y = element_line(color="black", size = 1))+
  theme(legend.position = "none")

#axis theme for plots
axistheme <- theme(plot.title = element_text(hjust = 0.5, color = "black", face = "bold", size=20)) +
  theme(axis.title = element_text(color = "black", size = 16)) +
  theme(axis.text.x = element_text(size = 14, color = "black")) +
  theme(axis.text.y = element_text(size = 14, color = "black"))

c.inner <- "snow4" # dotplot dot fill color
c.outer <- "#F6491A" # color for cutoff values
c.select <- "grey10" # color for selected sample 

man.fills <- factor(c(c.outer, c.inner),
                    levels = c(c.outer, c.inner))
man.color <- factor(c(c.outer, c.inner, c.select),
                    levels = c(c.outer, c.select, c.inner))


SimpleDotPlot <- function(data) {
  
  data <- data %>%
    arrange(samples)
  
  x.vals <- vector()
  y.vals <- vector()
  
  frequency <- table(data$samples)
  values <- as.numeric(names(frequency))
  
  for(i in 1:length(frequency)) {
    n <- frequency[[i]]
    x.i <- rep(values[i], n)
    y.i <- c(1:n)
    
    x.vals <- c(x.vals, x.i)
    y.vals <- c(y.vals, y.i)
  }
  
  data['x.vals'] <- x.vals
  data['y.vals'] <- y.vals
  
  return(data)
  
}