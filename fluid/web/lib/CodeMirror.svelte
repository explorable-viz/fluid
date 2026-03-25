<script lang="ts">
	import { loadCode } from '../../output-es/App.LoadFigure';
	import { onMount } from 'svelte';

	let { name, src, open = false } = $props();

	onMount(() => {
		loadCode(src)();
	});
</script>

<details {open}>
	<summary>{name}.fld</summary>
	<div id="codemirror-{name}"></div>
</details>
