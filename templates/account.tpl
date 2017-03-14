{{> templates/header.tpl}}

<h2>Create an account</h2>

<form method="post">
	Username: <input type="text" name="name" /><br />
	Password: <input type="text" name="pass" /><br />
	<br />
	Profile picture URL: <input type="text" name="pic" /><br />
	Short description: <input type="text" name="desc" /><br />
	<input type="submit" />
</form>

{{> templates/footer.tpl}}
