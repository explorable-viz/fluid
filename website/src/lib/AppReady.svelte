<script lang="ts">
	import { onMount } from 'svelte';

	// Signal hydration completion for the Puppeteer webtest (waits for body.app-ready).
	onMount(() => {
		document.body.classList.add('app-ready');
	});
</script>
