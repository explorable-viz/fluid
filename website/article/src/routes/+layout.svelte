<script lang="ts">
	import { onMount } from 'svelte';
	import '$lib/assets/css/styles.css';
	import '@fontsource/roboto/latin-300.css';
	import '@fontsource/roboto/latin-500.css';
	import '@fontsource/fira-code/400.css';

	import favicon from '$lib/assets/image/favicon.ico';

	let { children } = $props();

	onMount(() => {
		document.body.setAttribute('data-interactive', '');
	});
</script>

<svelte:head>
	<link rel="icon" href={favicon} />
</svelte:head>

{@render children()}
