{{> templates/header.tpl}}

<h2>Lastest videos</h2>

<div class="list">
	{{#list}}
	<article>
		<img src="thumb/{{id}}" /><br />
		<a href="/entry/{{id}}">{{title}}</a> by <a href="/author/{{author_id}}">{{author}}</a>
	</article>
	{{/list}}

	{{^list}}
	No entries :(
	{{/list}}
</div>

{{> templates/footer.tpl}}
