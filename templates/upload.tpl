{{> templates/header.tpl}}

<h2>Upload a video</h2>

<form method="post" enctype="multipart/form-data">
	Your username: <input type="text" name="name" /><br />
	Your password: <input type="text" name="pass" /><br />
	<br />
	Title: <input type="text" name="desc" /><br />
	Description:<br />
	<textarea name="desc" rows="5" cols="60"></textarea><br />
	<br />
	Your file: <input type="file" name="file" /><br />
	<br />
	<input type="submit" />
</form>

{{> templates/footer.tpl}}
