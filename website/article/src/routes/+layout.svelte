<script lang="ts">
	import '$lib/assets/css/styles.css';
	import '$lib/assets/css/view-styles.css';
	import '@fontsource/roboto/latin-300.css';
	import '@fontsource/roboto/latin-500.css';

	import favicon from '$lib/assets/image/favicon.ico';

	let { children } = $props();
</script>

<svelte:head>
	<link rel="icon" href={favicon} />
</svelte:head>

{@render children()}
