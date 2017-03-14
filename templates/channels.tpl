{{> templates/header.tpl}}

<h2>Channel list</h2>

{{#authors}}

	<h3><a href="/author/{{id}}">{{name}}</a></h3>
	<p>
	<img src="{{pic}}" class="profile_pic" /><br />
	{{desc}}<br />
	</p>

{{/authors}}


{{> templates/footer.tpl}}
