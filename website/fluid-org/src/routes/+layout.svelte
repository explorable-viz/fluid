<script lang="ts">
	import '$lib/assets/css/styles.css';
	import '@fontsource/roboto/latin-300.css';
	import '@fontsource/roboto/latin-500.css';
	import '@fontsource/fira-code/400.css';

	import { page } from '$app/state';
	import favicon from '$lib/assets/image/favicon.ico';
	import FluidLogo from '$lib/assets/image/fluid.png';
	import { headerLinks } from '$lib/config/nav';

	let { children } = $props();
</script>

<svelte:head>
	<link rel="icon" href={favicon} />
</svelte:head>

<div class="header-grid-container">
	<div class="flex-right-align right-border">
		<img class="fluid-logo" src={FluidLogo} width="150px" alt="Fluid logo" />
		<p class="fluid-subtitle">pure • pythonic • provenance-aware</p>
	</div>
	<div class="flex-left-align" style="justify-content: center; margin-left: 5px;">
		<nav>
			<ul>
				{#each headerLinks as { url, title }, index (index)}
					<li class={page.route.id === url ? 'active-page' : ''}><a href={url}>{title}</a></li>
				{/each}
			</ul>
		</nav>
		<div></div>
	</div>
</div>

{@render children()}

<div class="footer-grid-container">
	<div></div>
	<div class="flex-left-align">
		<p>© Fluid Contributors 2019-2026</p>
	</div>
</div>

<style>
	.fluid-logo {
		margin-bottom: 0;
	}

	.fluid-subtitle {
		margin-bottom: 0;
		margin-top: 0;
		font-size: 12pt;
	}
	.header-grid-container {
		display: grid;
		background-color: #f0f0f0;
		width: 100%;
		grid-template-columns: auto 1fr;
		padding-top: 7px;
		padding-bottom: 7px;
	}
	.footer-grid-container {
		display: grid;
		background-color: #f0f0f0;
		width: 100%;
		grid-template-columns: auto 1fr;
		margin-top: 10px;
	}

	@media (max-width: 600px) {
		.header-grid-container,
		.footer-grid-container {
			grid-template-columns: 1fr;
			text-align: center;
		}
	}
</style>
