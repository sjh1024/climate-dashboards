// <![CDATA[

/*
Triple Combo Script Credit
By Hamid Cheheltani/ JavaScript Kit (http://www.javascriptkit.com)
*/
 
var temp=document.arcsearch.sub_type_cat
var temp1=document.arcsearch.collect_method_cat
var temp2=document.arcsearch.sub_sub_type_cat
var CB2=document.arcsearch.sub_type_cat.options.length
var CB3=document.arcsearch.collect_method_cat.options.length
var CB4=document.arcsearch.sub_sub_type_cat.options.length
var ComboBox2=new Array(CB2)
var ComboBox3=new Array(CB2)
var ComboBox4=new Array(CB2)

var maximum="20"
var max=eval(maximum-1)

for (a=0; a<=max; a++){
ComboBox2[a]=new Array()
}
for (b=0; b<=max; b++){
ComboBox3[b]=new Array()
for (c=0; c<=max; c++){
ComboBox3[b][c]=new Array()
}}
for (d=0; d<=max; d++){
ComboBox4[d]=new Array()
for (e=0; e<=max; e++){
ComboBox4[d][e]=new Array()
for (f=0;f<=max; f++){
ComboBox4[d][e][f]=new Array()
}}}
ComboBox2[0][0]=new Option("N/A","");
ComboBox2[1][0]=new Option("Choose a subtype","");
ComboBox2[1][1]=new Option("All","");
ComboBox2[1][2]=new Option("Streamwater","101");
ComboBox2[1][3]=new Option("Precipitation","102");
ComboBox2[1][4]=new Option("Soil Solution","103");
ComboBox2[2][0]=new Option("Choose a subtype","");
ComboBox2[2][1]=new Option("All","");
ComboBox2[2][2]=new Option("Glacial Till","201");
ComboBox2[2][3]=new Option("Bedrock Cores","201");
ComboBox2[3][0]=new Option("Choose a subtype","");
ComboBox2[3][1]=new Option("All","");
ComboBox2[3][2]=new Option("Organic","301");
ComboBox2[3][3]=new Option("Mineral","302");
ComboBox2[3][4]=new Option("Debris","303");
ComboBox2[3][5]=new Option("Sediment","304");
ComboBox2[4][0]=new Option("All","");
ComboBox2[4][1]=new Option("Fruiting Body","401");
ComboBox2[5][0]=new Option("Choose a sub-type","");
ComboBox2[5][1]= new Option("All","");
ComboBox2[5][2]= new Option("Foliage","501");
ComboBox2[5][3]= new Option("Liter","502");
ComboBox2[5][4]= new Option("Twig","503");
ComboBox2[5][5]= new Option("Branch","504");
ComboBox2[5][6]= new Option("Wood","505");
ComboBox2[5][7]= new Option("Bark","506");
ComboBox2[5][8]= new Option("Stem","507");
ComboBox2[5][9]= new Option("Root","508");
ComboBox2[5][10]= new Option("Fruit","509");
ComboBox2[5][11]= new Option("Multiple Plant Parts","510");
ComboBox2[5][12]= new Option("Tree Core","511");
ComboBox2[5][13]= new Option("Bole Selection","512");
ComboBox2[5][14]= new Option("Other Plant Parts","513");
ComboBox2[6][0]=new Option("All","");
ComboBox2[6][1]=new Option("Multiple Insect Parts","601");
ComboBox2[7][0]=new Option("Other","701");
ComboBox3[0][0][0]=new Option("N/A","");
ComboBox3[1][0][0]=new Option("N/A","");
ComboBox3[2][0][0]=new Option("N/A","");
ComboBox3[3][0][0]=new Option("N/A","");
ComboBox3[3][1][0]=new Option("N/A","");
ComboBox3[3][2][0]=new Option("Choose a method","");
ComboBox3[3][2][1]=new Option("All","");
ComboBox3[3][2][2]=new Option("Grab","1");
ComboBox3[3][2][3]=new Option("Horizon","2");
ComboBox3[3][2][4]=new Option("Depth","3");
ComboBox3[3][3][0]=new Option("Choose a method","");
ComboBox3[3][3][1]=new Option("All","");
ComboBox3[3][3][2]=new Option("Grab","1");
ComboBox3[3][3][3]=new Option("Horizon","2");
ComboBox3[3][3][4]=new Option("Depth","3");
ComboBox3[3][4][0]=new Option("N/A","");
ComboBox3[3][5][0]=new Option("N/A","");
ComboBox3[4][0][0]=new Option("N/A","");
ComboBox3[4][1][0]=new Option("N/A","");
ComboBox3[5][0][0]=new Option("N/A","");
ComboBox3[6][0][0]=new Option("N/A","");
ComboBox3[6][1][0]=new Option("N/A","");
ComboBox3[7][0][0]=new Option("N/A","");
ComboBox4[0][0][0][0]=new Option("N/A","");
ComboBox4[1][0][0][0]=new Option("N/A","");
ComboBox4[2][0][0][0]=new Option("N/A","");
ComboBox4[3][0][0][0]=new Option("N/A","");
ComboBox4[3][1][0][0]=new Option("N/A","");
ComboBox4[3][2][0][0]=new Option("N/A","");
ComboBox4[3][2][1][0]=new Option("N/A","");
ComboBox4[3][2][2][0]=new Option("N/A","");
ComboBox4[3][2][3][0]=new Option("Choose a horizon","");
ComboBox4[3][2][3][1]=new Option("All","");
ComboBox4[3][2][3][2]=new Option("O","30101");
ComboBox4[3][2][3][3]=new Option("Oi","30102");
ComboBox4[3][2][3][4]=new Option("Oie","30103");
ComboBox4[3][2][3][5]=new Option("Oe","30104");
ComboBox4[3][2][3][6]=new Option("Oea","30105");
ComboBox4[3][2][3][7]=new Option("Oa","30106")
ComboBox4[3][2][4][0]=new Option("Choose a depth","");
ComboBox4[3][2][4][1]= new Option("All","");
ComboBox4[3][2][4][2]= new Option("0-10cm","30001");
ComboBox4[3][2][4][3]= new Option("10-20cm","30002");
ComboBox4[3][2][4][4]= new Option("20+cm","30003");
ComboBox4[3][2][4][5]= new Option("10-35cm","30004");
ComboBox4[3][2][4][6]= new Option("35-60cm","30005");
ComboBox4[3][2][4][7]= new Option("0-2cm","30006");
ComboBox4[3][2][4][8]= new Option("2-4cm","30007");
ComboBox4[3][2][4][9]= new Option("4-6cm","30008");
ComboBox4[3][2][4][10]= new Option("6-8cm","30009");
ComboBox4[3][2][4][11]= new Option("8-10cm","30010");
ComboBox4[3][2][4][12]= new Option("10-13cm","30011");
ComboBox4[3][2][4][13]= new Option("13-16cm","30012");
ComboBox4[3][2][4][14]= new Option("16-20cm","30013");
ComboBox4[3][2][4][15]= new Option("20-40cm","30014");
ComboBox4[3][3][0][0]=new Option("N/A","");
ComboBox4[3][3][1][0]=new Option("N/A","");
ComboBox4[3][3][2][0]=new Option("N/A","");
ComboBox4[3][3][3][0]=new Option("Choose a horizon","");
ComboBox4[3][3][3][1]=new Option("All","");
ComboBox4[3][3][3][2]= new Option("A","30201");
ComboBox4[3][3][3][3]= new Option("E","30202");
ComboBox4[3][3][3][4]= new Option("B","30203");
ComboBox4[3][3][3][5]= new Option("C","30204");
ComboBox4[3][3][3][6]= new Option("R","30205");
ComboBox4[3][3][3][7]= new Option("Multiple","30206");
ComboBox4[3][3][4][0]=new Option("Choose a depth","");
ComboBox4[3][3][4][1]= new Option("All","");
ComboBox4[3][3][4][2]= new Option("0-10cm","30001");
ComboBox4[3][3][4][3]= new Option("10-20cm","30002");
ComboBox4[3][3][4][4]= new Option("20+cm","30003");
ComboBox4[3][3][4][5]= new Option("10-35cm","30004");
ComboBox4[3][3][4][6]= new Option("35-60cm","30005");
ComboBox4[3][3][4][7]= new Option("0-2cm","30006");
ComboBox4[3][3][4][8]= new Option("2-4cm","30007");
ComboBox4[3][3][4][9]= new Option("4-6cm","30008");
ComboBox4[3][3][4][10]= new Option("6-8cm","30009");
ComboBox4[3][3][4][11]= new Option("8-10cm","30010");
ComboBox4[3][3][4][12]= new Option("10-13cm","30011");
ComboBox4[3][3][4][13]= new Option("13-16cm","30012");
ComboBox4[3][3][4][14]= new Option("16-20cm","30013");
ComboBox4[3][3][4][15]= new Option("20-40cm","30014");
ComboBox4[3][4][0][0]=new Option("N/A","");
ComboBox4[3][5][0][0]=new Option("N/A","");
ComboBox4[4][0][0][0]=new Option("N/A","");
ComboBox4[4][1][0][0]=new Option("N/A","");
ComboBox4[5][0][0][0]=new Option("N/A","");
ComboBox4[6][0][0][0]=new Option("N/A","");
ComboBox4[6][1][0][0]=new Option("N/A","");
ComboBox4[7][0][0][0]=new Option("N/A","");

function redirect(x){
for (m=temp.options.length-1;m>0;m--)
temp.options[m]=null
for (i=0;i<ComboBox2[x].length;i++){
temp.options[i]=new Option(ComboBox2[x][i].text,ComboBox2[x][i].value)
}
temp.options[0].selected=true
redirect1(0)
}
function redirect1(y){
var x=document.arcsearch.type_cat.options.selectedIndex
for (m=temp1.options.length-1;m>0;m--)
temp1.options[m]=null
for (i=0;i<ComboBox3[x][y].length;i++){
temp1.options[i]=new Option(ComboBox3[x][y][i].text,ComboBox3[x][y][i].value)
}
temp1.options[0].selected=true
redirect2(0)
}
function redirect2(z){
var x=document.arcsearch.type_cat.options.selectedIndex
var y=document.arcsearch.sub_type_cat.options.selectedIndex
for (m=temp2.options.length-1;m>0;m--)
temp2.options[m]=null
for (i=0;i<ComboBox4[x][y][z].length;i++){
temp2.options[i]=new Option(ComboBox4[x][y][z][i].text,ComboBox4[x][y][z][i].value)
}
temp2.options[0].selected=true
}

// ]]>