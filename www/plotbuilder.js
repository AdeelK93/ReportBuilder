function radioSet(a,b) {$('input[name="'+a+'"][value="'+b+'"]').prop("checked",!0)}

function modalBar() {
  updateEditor(modalQuery().concat("geom_bar(stat='summary',fun.data=mean_se) +\n geom_errorbar(stat='summary',fun.data=mean_se,size=.8,width=.2,color='black') +\n theme(legend.position='none')"));
  $('#shiny-modal').modal("hide");
}

function modalBox() {
  updateEditor(modalQuery().concat("geom_boxplot(color='black') +\n theme(legend.position='none')"));
  $('#shiny-modal').modal("hide");
}

function modalLine() {
  updateEditor(modalQuery().concat("geom_line(aes(group=Battery.ID))"));
  $('#shiny-modal').modal("hide");
}

function modalLine2() {
  updateEditor(modalQuery().concat("geom_line(stat='summary',fun.data=mean_se) +\n geom_ribbon(stat='summary',fun.data=mean_se,alpha=.3,color=NA,show.legend=F)"));
  $('#shiny-modal').modal("hide");
}

function modalQuery() {
  var q ="";
  switch ($("ul li.active").index()) {
    // Quick
    case 0:
      // add in a title if it's empty
      var modalT=modalID.replace('e','t');
      if (!document.getElementById(modalT).value.length) {
        var Title=$('input[name="modalq"]:checked').val();
        document.getElementById(modalT).value=Title;
         Shiny.onInputChange(modalT, Title);
      }
      switch ($('input[name="modalq"]:checked').val()) {
        case 'Discharge Capacity':
          q="summarize(DisCap=-min(Amp.Hours)) %>%\n ggplot(aes(Type,DisCap,color=Type,fill=Type)) + ylab('Discharge Capacity (Ah)')";
          break;
        case 'Reserve Capacity':
          q="filter(Amp.Hours==min(Amp.Hours)) %>%\n ggplot(aes(Type,Step.time/60,color=Type,fill=Type)) + ylab('Reserve Capacity (min)')";
          break;
        case 'Ah In During Charge':
          q="summarize(AhIn=max(Amp.Hours)) %>%\n ggplot(aes(Type,AhIn,color=Type,fill=Type)) + ylab('Ah In During Charge')";
          break;
        case '%Ah In During Charge':
          q="mutate(RF=-Amp.Hours/min(Amp.Hours)) %>% summarize(RFmax=max(RF)) %>%\n ggplot(aes(Type,RFmax*100,color=Type,fill=Type)) + ylab('% Ah In During Charge')";
          break;
        case 'Time to 100% Recharge Factor':
          q="mutate(RF=-Amp.Hours/min(Amp.Hours)) %>% summarize(TimeTo100=Step.time[which.min(abs(RF-1))]) %>%\n ggplot(aes(Type,TimeTo100/3600,color=Type,fill=Type)) + ylab('Time to 100% Ah In During Charge (hr)')";
          break;
        case 'CCA Duration':
          q="filter(Current<0) %>% summarize(Duration=max(Step.time)) %>%\n ggplot(aes(Type,Duration,color=Type,fill=Type)) + ylab('CCA Duration (s)')";
          break;
        case 'Voltage After 30s':
          q="filter(Current<0) %>% summarize(V30s=Voltage[which.min(abs(Step.time-30))]) %>%\n ggplot(aes(Type,V30s,color=Type,fill=Type)) + ylab('Voltage After 30s of Discharge')";
          break;
        case 'Time to 6V':
          q="filter(Current<0) %>% summarize(TimeTo6V=Step.time[which.min(abs(Voltage-6))]) %>%\n ggplot(aes(Type,TimeTo6V,color=Type,fill=Type)) + ylab('Discharge Time to 6V (s)')";
          break;
        case 'Current at 10m':
          q="filter(Current>0) %>% summarize(Cur10m=Current[which.min(abs((Step.time/60)-10))]) %>%\n ggplot(aes(Type,Cur10m,color=Type,fill=Type)) + ylab('Current at 10m')";
          break;
        case 'Ah In at 10m':
          q="filter(Current>0) %>% summarize(Ah10m=Amp.Hours[which.min(abs((Step.time/60)-10))]) %>%\n ggplot(aes(Type,Ah10m,color=Type,fill=Type)) + ylab('Ah In at 10m')";
          break;
        case 'Minimum Discharge Voltage':
          q="summarize(MinVolt=min(Voltage)) %>%\n ggplot(aes(Cycle,MinVolt,color=Type,fill=Type)) + ylab('Minimum Discharge Voltage')";
          break;
        case 'BOC Voltage':
          q="filter(Step.time==min(Step.time)) %>%\n ggplot(aes(Cycle,Voltage,color=Type,fill=Type)) + ylab('Beginning of Charge Voltage')";
          break;
        case 'EOC Voltage':
          q="filter(Step.time==max(Step.time)) %>%\n ggplot(aes(Cycle,Voltage,color=Type,fill=Type)) + ylab('End of Charge Voltage')";
          break;
        case 'BOC Current':
          q="filter(Step.time==min(Step.time)) %>%\n ggplot(aes(Cycle,Current,color=Type,fill=Type)) + ylab('Beginning of Charge Current')";
          break;
        case 'EOC Current':
          q="filter(Step.time==min(Step.time)) %>%\n ggplot(aes(Cycle,Current,color=Type,fill=Type)) + ylab('End of Charge Current')";
          break;
        default:
          console.error("Could not find query selector");
      }
      break;
      
    // Continuous
    case 1:
      q="ggplot(aes(".concat(document.getElementById('modalcx').value,",",document.getElementById('modalcy').value,",color=Type,fill=Type))");
      break;
      
    // Discrete
    case 2:
      var dx=document.getElementById('modaldx').value; // known
      var dv=document.getElementById('modaldv').value; // known value
      var dy=document.getElementById('modaldy').value; // unknown
      if (!isNaN(parseInt(dv))) {
        q="summarize(Variable=".concat(dy,"[which.min(abs(",dx,"-",dv,"))]) %>%\n ggplot(aes(Type,Variable,color=Type,fill=Type))");
      } else {
        if (dx==dy) {
          q="summarize(Variable=".concat(dv,"(",dx,")) %>%\n ggplot(aes(Type,Variable,color=Type,fill=Type))");
        } else {
          q="filter(".concat(dx,"==",dv,"(",dx,")) %>%\n ggplot(aes(Type,",dy,",color=Type,fill=Type))");
        }
      }
      break;
    
    default:
      console.error("Could not find tab index");
  }
  return "x %>% group_by(Battery.ID,Type,Cycle) %>%\n ".concat(modalFilter(),q," +\n ");
}

function modalFilter() {
  var voltageV=$('input[id="filter.voltage"]')[0].value.split(";");
  var voltageD=$('input[id="filter.voltage"]')[0].dataset;
  var currentV=$('input[id="filter.current"]')[0].value.split(";");
  var currentD=$('input[id="filter.current"]')[0].dataset;
  var amphoursV=$('input[id="filter.amphours"]')[0].value.split(";");
  var amphoursD=$('input[id="filter.amphours"]')[0].dataset;
  var totaltimeV=$('input[id="filter.totaltime"]')[0].value.split(";");
  var totaltimeD=$('input[id="filter.totaltime"]')[0].dataset;
  var steptimeV=$('input[id="filter.steptime"]')[0].value.split(";");
  var steptimeD=$('input[id="filter.steptime"]')[0].dataset;
  var stepV=$('input[id="filter.step"]')[0].value;
  var f="";
  
  if (voltageV[0]!=voltageD.min) {
    f=f.concat("Voltage>=",voltageV[0],",");
  }
  if (voltageV[1]!=voltageD.max) {
    f=f.concat("Voltage<=",voltageV[1],",");
  }
  if (currentV[0]!=currentD.min) {
    f=f.concat("Current>=",currentV[0],",");
  }
  if (currentV[1]!=currentD.max) {
    f=f.concat("Current<=",currentV[1],",");
  }
  if (amphoursV[0]!=amphoursD.min) {
    f=f.concat("Amp.Hours>=",amphoursV[0],",");
  }
  if (amphoursV[1]!=amphoursD.max) {
    f=f.concat("Amp.Hours<=",amphoursV[1],",");
  }
  if (totaltimeV[0]!=totaltimeD.min) {
    f=f.concat("Total.Time>=",totaltimeV[0],",");
  }
  if (totaltimeV[1]!=totaltimeD.max) {
    f=f.concat("Total.Time<=",totaltimeV[1],",");
  }
  if (steptimeV[0]!=steptimeD.min) {
    f=f.concat("Step.time>=",steptimeV[0],",");
  }
  if (steptimeV[1]!=steptimeD.max) {
    f=f.concat("Step.time<=",steptimeV[1],",");
  }
  if (stepV!=0) {
    f=f.concat("Step==",stepV,",");
  }
  
  if (f.length) {
    f=f.substring(0,f.length-1); //remove training comma
    f="filter(".concat(f,") %>%\n ");
  }
  return f;
}