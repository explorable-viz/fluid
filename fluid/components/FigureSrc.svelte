<script lang="ts">
	import { loadFigureSrc } from '$lib/fluid/App.LoadFigure';
	import { onMount } from 'svelte';

	let { spec, src } = $props();

	onMount(() => {
		loadFigureSrc(spec)('fig')(src)();
	});
</script>

<div id="fig">
	<div class="fig-loading">loading figure(s)</div>
</div>

<style>
	@keyframes fadeIn {
		from {
			opacity: 0;
		}
		to {
			opacity: 1;
		}
	}

	.fig-loading:only-child {
		color: fuchsia;
		animation: fadeIn 2s ease-in-out infinite alternate;
	}

	.fig-loading:not(:only-child) {
		display: none;
	}
</style>
