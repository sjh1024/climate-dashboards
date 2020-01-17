function meFocus(id)
{
	document.getElementById(id).style['backgroundColor'] = '#ffffff';
}
function meBlur(id)
{
	if (document.getElementById(id).value == "") 
		document.getElementById(id).style['backgroundColor'] = '#EEF7E6';
	else
		document.getElementById(id).style['backgroundColor'] = '#ffffff';
}