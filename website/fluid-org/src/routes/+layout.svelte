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

<div class="site-header">
	<div class="header-logo">
		<img class="fluid-logo" src={FluidLogo} width="150px" alt="Fluid logo" />
		<p class="fluid-subtitle">pure • pythonic • provenance-aware</p>
	</div>
	<div class="header-divider"></div>
	<div class="header-nav">
		<nav>
			<ul>
				{#each headerLinks as { url, title }, index (index)}
					<li class={page.route.id === url ? 'active-page' : ''}><a href={url}>{title}</a></li>
				{/each}
			</ul>
		</nav>
	</div>
</div>

{@render children()}

<div class="site-footer">
	<div></div>
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

	.site-header, .site-footer {
		display: grid;
		background-color: #f0f0f0;
		width: 100%;
		grid-template-columns: auto var(--toggle-button-width) 800px;
		justify-content: center;
		padding-top: 7px;
		padding-bottom: 7px;
	}

	.site-footer {
		margin-top: 10px;
	}

	.header-logo {
		display: flex;
		flex-direction: column;
		align-items: flex-end;
		justify-content: center;
		padding-right: 10px;
	}

	.header-divider {
		border-right: 1px solid #ccc;
	}

	.header-nav {
		display: flex;
		align-items: center;
		padding-left: 10px;
	}

	@media (max-width: 900px) {
		.site-header, .site-footer {
			grid-template-columns: 1fr;
			text-align: center;
		}

		.header-logo {
			align-items: center;
			padding-right: 0;
		}

		.header-divider {
			display: none;
		}

		.header-nav {
			justify-content: center;
			padding-left: 0;
		}
	}
</style>
