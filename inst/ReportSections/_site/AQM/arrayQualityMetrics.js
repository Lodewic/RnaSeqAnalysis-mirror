// (C) Wolfgang Huber 2010-2011

// Script parameters - these are set up by R in the function 'writeReport' when copying the 
//   template for this script from arrayQualityMetrics/inst/scripts into the report.

var highlightInitial = [ true, true, false, false, false, false, true, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false, false, true, true, false, false, false, false, false, false, false, false, false, true, false, false, false, true, false, true, true, true, false, false, false, true, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true ];
var arrayMetadata    = [ [ "1", "X466", "chow_00wk", "chow", "00wk", "0", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "2", "X468", "chow_00wk", "chow", "00wk", "0", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "3", "X471", "chow_00wk", "chow", "00wk", "0", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "4", "X472", "chow_12wk", "chow", "12wk", "12", "Chow", "12", "95566.73201", "23891.683", "21585.89521", "2389.1683" ], [ "5", "X473", "chow_12wk", "chow", "12wk", "12", "Chow", "12", "178516.1135", "44629.02837", "35940.6339", "2479.390465" ], [ "6", "X474", "chow_06wk", "chow", "06wk", "6", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "7", "X475", "chow_12wk", "chow", "12wk", "12", "Chow", "12", "48080.75303", "12020.18826", "3667.807896", "924.6298659" ], [ "8", "X476", "chow_06wk", "chow", "06wk", "6", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "9", "X477", "chow_06wk", "chow", "06wk", "6", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "10", "X478", "HFD_06wk", "HFD", "06wk", "6", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "11", "X480", "HFD_06wk", "HFD", "06wk", "6", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "12", "X481", "HFD_06wk", "HFD", "06wk", "6", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "13", "X482", "HFD_06wk", "HFD", "06wk", "6", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "14", "X484", "HFD_18wk", "HFD", "18wk", "18", "HFD", "18", "268224.4042", "67056.10105", "47664.57733", "3944.476532" ], [ "15", "X485", "HFD_12wk", "HFD", "12wk", "12", "HFD", "12", "309365.9882", "77341.49706", "62298.11636", "3362.673785" ], [ "16", "X486", "HFD_12wk", "HFD", "12wk", "12", "HFD", "12", "120128.1588", "30032.03971", "21552.56378", "1766.590571" ], [ "17", "X487", "HFD_18wk", "HFD", "18wk", "18", "HFD", "18", "617499.2325", "154374.8081", "143418.5261", "6432.283672" ], [ "18", "X489", "HFD_12wk", "HFD", "12wk", "12", "HFD", "12", "366410.7172", "91602.67929", "84631.32107", "5089.037738" ], [ "19", "X490", "chow_24wk", "chow", "24wk", "24", "Chow", "24", "137488.7493", "41246.62479", "22657.90067", "2946.187485" ], [ "20", "X495", "chow_24wk", "chow", "24wk", "24", "Chow", "24", "167257.7718", "41814.44295", "25192.75123", "2459.673115" ], [ "21", "X496", "HFD_06wk", "HFD", "06wk", "6", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "22", "X497", "HFD_06wk", "HFD", "06wk", "6", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "23", "X499", "HFD_06wk", "HFD", "06wk", "6", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "24", "X500", "HFD_06wk", "HFD", "06wk", "6", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "25", "X503", "HFD_06wk", "HFD", "06wk", "6", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "26", "X505", "HFD_06wk", "HFD", "06wk", "6", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "27", "X506", "HFD_06wk", "HFD", "06wk", "6", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "28", "X507", "HFD_06wk", "HFD", "06wk", "6", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "29", "X515", "HFD_12wk", "HFD", "12wk", "12", "HFD", "12", "44176.92533", "11044.23133", "9344.224231", "1840.705222" ], [ "30", "X516", "HFD_12wk", "HFD", "12wk", "12", "HFD", "12", "122305.8127", "30576.45317", "24453.12561", "2038.430211" ], [ "31", "X518", "HFD_12wk", "HFD", "12wk", "12", "HFD", "12", "143513.3286", "35878.33215", "22685.31344", "1888.333271" ], [ "32", "X519", "HFD_12wk", "HFD", "12wk", "12", "HFD", "12", "238955.4571", "59738.86428", "52967.287", "4595.297252" ], [ "33", "X524", "chow_18wk", "chow", "18wk", "18", "Chow", "18", "189699.6931", "47424.92327", "42471.93409", "3648.07102" ], [ "34", "X525", "chow_18wk", "chow", "18wk", "18", "Chow", "18", "74143.65204", "20220.99601", "16994.55826", "1838.272365" ], [ "35", "X532", "HFD_18wk", "HFD", "18wk", "18", "HFD", "18", "877584.536", "219396.134", "218277.7137", "13712.25838" ], [ "36", "X533", "HFD_18wk", "HFD", "18wk", "18", "HFD", "18", "603929.9112", "181178.9734", "160911.8401", "12078.59822" ], [ "37", "X535", "HFD_18wk", "HFD", "18wk", "18", "HFD", "18", "1982467.041", "495616.7602", "477023.6335", "38124.36617" ], [ "38", "X536", "HFD_18wk", "HFD", "18wk", "18", "HFD", "18", "1625531.174", "406382.7935", "400578.1394", "18471.94516" ], [ "39", "X537", "HFD_18wk", "HFD", "18wk", "18", "HFD", "18", "1039077.319", "259769.3298", "252505.0495", "10390.77319" ], [ "40", "X539", "HFD_12wk", "HFD", "12wk", "12", "HFD", "12", "120164.7092", "32772.19342", "23612.92987", "2340.870958" ], [ "41", "X540", "HFD_12wk", "HFD", "12wk", "12", "HFD", "12", "263409.3096", "65852.32739", "63872.7931", "3873.666317" ], [ "42", "X541", "HFD_12wk", "HFD", "12wk", "12", "HFD", "12", "130192.3836", "32548.09591", "29754.79692", "2958.91781" ], [ "43", "X546", "chow_24wk", "chow", "24wk", "24", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "44", "X548", "HFD_12wk", "HFD", "12wk", "12", "HFD", "12", "354006.0233", "88501.50582", "82404.03348", "3847.891557" ], [ "45", "X549", "HFD_12wk", "HFD", "12wk", "12", "HFD", "12", "172565.4656", "43141.3664", "34051.0218", "2396.742578" ], [ "46", "X550", "HFD_24wk", "HFD", "24wk", "24", "HFD", "24", "1626245.983", "406561.4957", "406561.4957", "20328.07478" ], [ "47", "X551", "HFD_24wk", "HFD", "24wk", "24", "HFD", "24", "1706440.381", "426610.0952", "410926.8755", "23700.56084" ], [ "48", "X552", "HFD_24wk", "HFD", "24wk", "24", "HFD", "24", "1597053.044", "435559.921", "398283.3111", "21777.99605" ], [ "49", "X553", "HFD_24wk", "HFD", "24wk", "24", "HFD", "24", "2258231.203", "564557.8008", "558442.4686", "29713.56846" ], [ "50", "X556", "HFD_24wk", "HFD", "24wk", "24", "HFD", "24", "1253429.4", "313357.3499", "300369.8231", "12052.20577" ], [ "51", "X557", "HFD_24wk", "HFD", "24wk", "24", "HFD", "24", "1605527.743", "401381.9358", "375890.3439", "19113.42552" ], [ "52", "X558", "HFD_24wk", "HFD", "24wk", "24", "HFD", "24", "2126697.67", "797511.6264", "788115.1206", "72501.05694" ], [ "53", "X560", "HFD_24wk", "HFD", "24wk", "24", "HFD", "24", "705409.9101", "423245.946", "335403.4681", "84649.18921" ], [ "54", "X561", "HFD_24wk", "HFD", "24wk", "24", "HFD", "24", "2213302.088", "737767.3626", "737767.3626", "122961.2271" ], [ "55", "X562", "chow_24wk", "chow", "24wk", "24", "Chow", "24", "191242.6998", "47810.67494", "32885.25221", "2656.148608" ], [ "56", "X563", "chow_24wk", "chow", "24wk", "24", "Chow", "24", "279712.0163", "69928.00408", "49731.64558", "3178.54564" ], [ "57", "X564", "chow_00wk", "chow", "00wk", "0", "Chow", "24", "211411.8532", "52852.96329", "37110.4531", "2936.275738" ], [ "58", "X565", "chow_24wk", "chow", "24wk", "24", "Chow", "24", "551024.5106", "137756.1277", "131206.6561", "10596.6252" ], [ "59", "X568", "HFD_18wk", "HFD", "18wk", "18", "HFD", "18", "811809.2551", "202952.3138", "194492.1514", "9664.395894" ], [ "60", "X569", "HFD_18wk", "HFD", "18wk", "18", "HFD", "18", "1267100.48", "316775.1201", "291571.1546", "13772.83131" ], [ "61", "X570", "HFD_18wk", "HFD", "18wk", "18", "HFD", "18", "818188.5391", "204547.1348", "197869.3216", "13636.47565" ], [ "62", "X571", "HFD_18wk", "HFD", "18wk", "18", "HFD", "18", "330331.358", "90090.37037", "71558.77128", "4095.016835" ], [ "63", "X572", "HFD_18wk", "HFD", "18wk", "18", "HFD", "18", "675257.9395", "168814.4849", "166395.5156", "10550.9053" ], [ "64", "X574", "chow_06wk", "chow", "06wk", "6", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "65", "X575", "chow_06wk", "chow", "06wk", "6", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "66", "X576", "chow_12wk", "chow", "12wk", "12", "Chow", "12", "11524.57783", "3143.066681", "837.2222525", "392.8833351" ], [ "67", "X577", "chow_06wk", "chow", "06wk", "6", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "68", "X578", "chow_12wk", "chow", "12wk", "12", "Chow", "12", "39256.33307", "9814.083267", "7511.099055", "981.4083267" ], [ "69", "X579", "chow_12wk", "chow", "12wk", "12", "Chow", "12", "60642.86268", "15160.71567", "9329.687156", "721.9388414" ], [ "70", "X581", "chow_00wk", "chow", "00wk", "0", "NA", "NA", "NA", "NA", "NA", "NA" ], [ "71", "X583", "HFD_24wk", "HFD", "24wk", "24", "HFD", "24", "931745.5246", "232936.3811", "207114.0707", "8032.289005" ], [ "72", "X585", "HFD_24wk", "HFD", "24wk", "24", "HFD", "24", "835896.358", "208974.0895", "171201.4726", "8358.96358" ], [ "73", "X586", "HFD_24wk", "HFD", "24wk", "24", "HFD", "24", "725748.9396", "217724.6819", "209953.4107", "15551.76299" ], [ "74", "X596", "chow_18wk", "chow", "18wk", "18", "Chow", "18", "147203.2536", "36800.8134", "23513.19987", "2453.38756" ], [ "75", "X597", "chow_18wk", "chow", "18wk", "18", "Chow", "18", "51006.44324", "12751.61081", "3697.920408", "910.8293435" ], [ "76", "X598", "chow_18wk", "chow", "18wk", "18", "Chow", "18", "73226.98561", "18306.7464", "14816.81001", "1408.211262" ], [ "77", "X599", "chow_18wk", "chow", "18wk", "18", "Chow", "18", "83468.97807", "20867.24452", "14507.17031", "1391.149635" ], [ "78", "X600", "chow_00wk", "chow", "00wk", "0", "NA", "NA", "NA", "NA", "NA", "NA" ] ];
var svgObjectNames   = [ "pca", "dens" ];

var cssText = ["stroke-width:1; stroke-opacity:0.4",
               "stroke-width:3; stroke-opacity:1" ];

// Global variables - these are set up below by 'reportinit'
var tables;             // array of all the associated ('tooltips') tables on the page
var checkboxes;         // the checkboxes
var ssrules;


function reportinit() 
{
 
    var a, i, status;

    /*--------find checkboxes and set them to start values------*/
    checkboxes = document.getElementsByName("ReportObjectCheckBoxes");
    if(checkboxes.length != highlightInitial.length)
	throw new Error("checkboxes.length=" + checkboxes.length + "  !=  "
                        + " highlightInitial.length="+ highlightInitial.length);
    
    /*--------find associated tables and cache their locations------*/
    tables = new Array(svgObjectNames.length);
    for(i=0; i<tables.length; i++) 
    {
        tables[i] = safeGetElementById("Tab:"+svgObjectNames[i]);
    }

    /*------- style sheet rules ---------*/
    var ss = document.styleSheets[0];
    ssrules = ss.cssRules ? ss.cssRules : ss.rules; 

    /*------- checkboxes[a] is (expected to be) of class HTMLInputElement ---*/
    for(a=0; a<checkboxes.length; a++)
    {
	checkboxes[a].checked = highlightInitial[a];
        status = checkboxes[a].checked; 
        setReportObj(a+1, status, false);
    }

}


function safeGetElementById(id)
{
    res = document.getElementById(id);
    if(res == null)
        throw new Error("Id '"+ id + "' not found.");
    return(res)
}

/*------------------------------------------------------------
   Highlighting of Report Objects 
 ---------------------------------------------------------------*/
function setReportObj(reportObjId, status, doTable)
{
    var i, j, plotObjIds, selector;

    if(doTable) {
	for(i=0; i<svgObjectNames.length; i++) {
	    showTipTable(i, reportObjId);
	} 
    }

    /* This works in Chrome 10, ssrules will be null; we use getElementsByClassName and loop over them */
    if(ssrules == null) {
	elements = document.getElementsByClassName("aqm" + reportObjId); 
	for(i=0; i<elements.length; i++) {
	    elements[i].style.cssText = cssText[0+status];
	}
    } else {
    /* This works in Firefox 4 */
	var success = false;
	i = 0; 
	/* Some of this looping could already be cached in reportInit() */
	while( (!success) & (i < ssrules.length) ) {
	    selector = ssrules[i].selectorText;  // The selector 
            if (!selector) 
		continue; // Skip @import and other nonstyle rules
            if (selector == (".aqm" + reportObjId)) {
		success = true; 
		ssrules[i].style.cssText = cssText[0+status];
	    } else {
		i++;
	    }
	}
    }

}

/*------------------------------------------------------------
   Display of the Metadata Table
  ------------------------------------------------------------*/
function showTipTable(tableIndex, reportObjId)
{
    var rows = tables[tableIndex].rows;
    var a = reportObjId - 1;

    if(rows.length != arrayMetadata[a].length)
	throw new Error("rows.length=" + rows.length+"  !=  arrayMetadata[array].length=" + arrayMetadata[a].length);

    for(i=0; i<rows.length; i++) 
 	rows[i].cells[1].innerHTML = arrayMetadata[a][i];
}

function hideTipTable(tableIndex)
{
    var rows = tables[tableIndex].rows;

    for(i=0; i<rows.length; i++) 
 	rows[i].cells[1].innerHTML = "";
}


/*------------------------------------------------------------
  From module 'name' (e.g. 'density'), find numeric index in the 
  'svgObjectNames' array.
  ------------------------------------------------------------*/
function getIndexFromName(name) 
{
    var i;
    for(i=0; i<svgObjectNames.length; i++)
        if(svgObjectNames[i] == name)
	    return i;

    throw new Error("Did not find '" + name + "'.");
}


/*------------------------------------------------------------
  SVG plot object callbacks
  ------------------------------------------------------------*/
function plotObjRespond(what, reportObjId, name)
{

    var a, i, status;

    switch(what) {
    case "show":
	i = getIndexFromName(name);
	showTipTable(i, reportObjId);
	break;
    case "hide":
	i = getIndexFromName(name);
	hideTipTable(i);
	break;
    case "click":
        a = reportObjId - 1;
	status = !checkboxes[a].checked;
	checkboxes[a].checked = status;
	setReportObj(reportObjId, status, true);
	break;
    default:
	throw new Error("Invalid 'what': "+what)
    }
}

/*------------------------------------------------------------
  checkboxes 'onchange' event
------------------------------------------------------------*/
function checkboxEvent(reportObjId)
{
    var a = reportObjId - 1;
    var status = checkboxes[a].checked;
    setReportObj(reportObjId, status, true);
}


/*------------------------------------------------------------
  toggle visibility
------------------------------------------------------------*/
function toggle(id){
  var head = safeGetElementById(id + "-h");
  var body = safeGetElementById(id + "-b");
  var hdtxt = head.innerHTML;
  var dsp;
  switch(body.style.display){
    case 'none':
      dsp = 'block';
      hdtxt = '-' + hdtxt.substr(1);
      break;
    case 'block':
      dsp = 'none';
      hdtxt = '+' + hdtxt.substr(1);
      break;
  }  
  body.style.display = dsp;
  head.innerHTML = hdtxt;
}
