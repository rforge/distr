
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body style="background-image: url(papier.gif);">

<! --- R-Forge Logo --- >
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<!-- <p> No content added. </p> -->
<div style="text-align: left;"><span style="color: rgb(0, 0, 0);"></span>
<h1><span style="color: rgb(0, 0, 0);"><a name="top"></a> <a
 href="http://www.uni-bayreuth.de/departments/math/org/mathe7/mathe7.html"><img
 style="border: 0px solid ; width: 198px; height: 127px;" src="logo.gif"
 alt="[Picture: logo]"></a>&nbsp; <big style="color: rgb(0, 0, 153);"><big
 style="color: rgb(0, 0, 153);"><span style="color: rgb(51, 51, 255);"><small><span
 style="color: rgb(0, 0, 153);">The</span></small> "distrXXX"-</span></big>Family
of R-Packages&nbsp;<span style="color: rgb(51, 51, 255);"></span></big>
<a href="http://www.r-project.org/"> <img
 style="border: 0px solid ; width: 152px; height: 101px;" alt="R-Logo"
 src="RLOGO.jpg"></a></span></h1>
</div>
<hr style="width: 100%; height: 2px;">
<div style="text-align: justify;"> Version: 2.0<br>
Release Date: 2008-09-05 <br>
Authors: <a
 href="mailto:peter.ruckdeschel@uni-bayreuth.de?subject=%5Bdistr%5D">Peter
Ruckdeschel</a>, <a
 href="mailto:matthias.kohl@stamats.de?subject=%5Bdistr%5D">Matthias
Kohl</a>, <a href="mailto:statho3@web.de?subject=%5Bdistr%5D">Thomas
Stabla</a>, <a href="mailto:fcampi@gmx.de?subject=%5Bdistr%5D">Florian
Camphausen</a>, <a href="mailto:eleonoragerber@gmx.de">Eleonara Feist</a>,
<a href="mailto:anja_h86@web.de">Anja Hueller</a>
<br>
Required R-Version: <br>
<ul>
  <li>&gt;=2.2.0 for versions 1.6-2.0</li>
</ul>
<hr style="width: 100%; height: 2px;">
<h2>What is the "distrXXX" family meant for?</h2>
<div style="text-align: justify; color: rgb(0, 0, 0);">Project <span
 style="color: rgb(102, 102, 0);"><big><big><span
 style="font-weight: bold;">distr</span></big></big> </span>is an
umbrella project which contains all the packages of the "<span
 style="color: rgb(51, 102, 255);">distrXXX</span>"-family. These in
return are all based on package "<span style="color: rgb(51, 102, 255);">distr</span>".
This family currently comprises the packages<br>
<ul>
  <li><a href="distr.html"><span style="color: rgb(51, 102, 255);">distr</span></a><br>
  </li>
  <li><a href="distrEx.html"><span style="color: rgb(51, 102, 255);">distrEx</span></a><br>
  </li>
  <li><a href="distrSim.html"><span style="color: rgb(51, 102, 255);">distrSim</span></a><br>
  </li>
  <li><a href="distrTEst.html"><span style="color: rgb(51, 102, 255);">distrTEst</span></a><br>
  </li>
  <li><a href="distrDoc.html"><span style="color: rgb(51, 102, 255);">distrDoc</span></a><br>
    <span style="color: rgb(51, 102, 255);"></span></li>
  <li><a href="distrMod.html"><span style="color: rgb(51, 102, 255);">distrMod</span></a></li>
  <li><a href="distrTeach.html"><span style="color: rgb(51, 102, 255);">distrTeach</span></a></li>
</ul>
It also contains package <span style="color: rgb(51, 102, 255);">startupmsg</span><br>
Besides the packages the project also contains the following folders<br>
<ul>
<li>pkg/Mail<br>
this folder contains important mails sent to Mailing lists R-devel and
R-packages<br>
</li>
<li>pkg/utils contains <br>
</li>
<ul>
<li>some utility R-functions for package development under Windows <br>
--- see <a
href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/utils/README-R-utils?root=distr&amp;view=markup">README-R-utils</a>&nbsp;</li>
<li>some utility batch files for building packages under various
versions of R under Windows<br>
--- see <a
href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/pkg/utils/README-batchfiles?root=distr&amp;view=markup">README-batchfiles</a></li>
</ul>
</ul>
Also the informative (non-formatted ASCII-)-Files<br>
<ul>
<li><a
href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/www/NEWS?root=distr&amp;view=markup">NEWS</a>
[a global, detailed NEWS for the whole
distrXXX project for developpers upto version 2.0]<a href="NEWS"><br>
</a></li>
<li><a
href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/www/WISHLIST?root=distr&amp;view=markup">WISHLIST</a>
(partly in German) </li>
<li><a
href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/www/TOBEDONE-PROJEKTE?root=distr&amp;view=markup">TOBEDONE-PROJEKTE</a>
(partly in
German) </li>
</ul>
should be interesting.<br>
</div>
<br>
If you want to <b>collaborate</b> (which you are welcome to do!):<br>
<ul>
<li> please read <a
href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/www/HOWTO-collaborate.txt?root=distr&amp;view=markup">HOWTO-collaborate.txt</a>
 [a short HOWTO for svn and R-Forge in 10]<br>
</a></li>
</ul>
<br>
</div>
<br><br>
<ul>
  <span style="color: rgb(51, 102, 255);"></span>
</ul>
<hr style="width: 100%; height: 2px; color: rgb(0, 0, 0);">
<div style="text-align: justify; color: rgb(0, 0, 0);">This page is
maintained by <a
 href="mailto:peter.ruckdeschel@itwm.fraunhofer.de?subject=distr-package">Peter
Ruckdeschel</a>
and last updated on 2008-08-28.<br>
</div>

<p> The <strong>project summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
