{{> templates/header.tpl}}

{{#list}}

<h2>{{title}}</h2>

<video autoplay controls>
	<source src="/file/{{id}}" type="video/ogg" />
</video>

<p>By <a href="/author/{{author_id}}">{{author}}</a> on {{date}} / Viewed {{views}} times</p>
<p>{{desc}}</p>

{{/list}}

{{> templates/footer.tpl}}
