<script lang="ts">
	import '$lib/assets/css/styles.css';
	import '$lib/assets/css/view-styles.css';
	import '@fontsource/roboto/latin-300.css';
	import '@fontsource/roboto/latin-500.css';

	import { page } from '$app/state';
	import favicon from '$lib/assets/image/favicon.ico';
	import FluidLogo from '$lib/assets/image/fluid.png';
	import { headerLinks } from '$lib/config/nav';

	let { children } = $props();
</script>

<svelte:head>
	<link rel="icon" href={favicon} />
</svelte:head>

{@render children()}
