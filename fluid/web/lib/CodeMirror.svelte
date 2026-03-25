<script lang="ts">
	import { loadCode } from '../../output-es/App.LoadFigure';
	import { onMount } from 'svelte';

	let { name, src, details = true } = $props();

	onMount(() => {
		loadCode(src)();
	});
</script>

{#if details}
<details>
	<summary>{name}.fld</summary>
	<div id="codemirror-{name}"></div>
</details>
{:else}
<div id="codemirror-{name}"></div>
{/if}
