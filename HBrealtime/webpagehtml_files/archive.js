// <![CDATA[

	function change() {
		if (document.getElementById('type_cat').selected=true)
				{
			document.getElementById('sub_type_cat').style.backgroundColor="#EEF7E6";
			document.getElementById('sub_type_cat').disabled=false; 
				}
			else
				{
				document.getElementById('sub_type_cat').style.backgroundColor="#C0C0C0";
				document.getElementById('sub_type_cat').disabled=true;
				}
		}

	function changetwo() {
	if (document.getElementById('sub_type_cat').selected=true)
			{
			document.getElementById('collect_method_cat').style.backgroundColor="#EEF7E6";
			document.getElementById('collect_method_cat').disabled=false; 
			}
			else
				{
				document.getElementById('collect_method_cat').style.backgroundColor="#C0C0C0";
				document.getElementById('collect_method_cat').disabled=true;
				}
	}



function changethree() {	
	if (document.getElementById('collect_method_cat').selected=true)
			{
			document.getElementById('sub_sub_type_cat').style.backgroundColor="#EEF7E6";
			document.getElementById('sub_sub_type_cat').disabled=false; 
			}
			else
				{
				document.getElementById('sub_sub_type_cat').style.backgroundColor="#C0C0C0";
				document.getElementById('sub_sub_type_cat').disabled=true;
				}
		}
 
 
// ]]>