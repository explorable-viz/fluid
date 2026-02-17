<script lang="ts">
	import { page } from '$app/state';

	const pages = [
		{ url: '/', title: 'Transparent research outputs' },
		{ url: '/convolution', title: 'Matrix convolution' },
		{ url: '/moving-average', title: 'Moving average' }
	];
</script>

<div>
	<h3 class="title">Overview</h3>
	<nav class="sub-header">
		<ul>
			{#each pages as { url, title }, index (index)}
				<li class={page.route.id === url ? 'active-page' : ''}><a href={url}>{title}</a></li>
			{/each}
		</ul>
	</nav>
</div>
<div></div>
<div></div>

<style>
	.sub-header * {
		font-size: 16pt;
		margin-bottom: 4px;
	}
</style>
