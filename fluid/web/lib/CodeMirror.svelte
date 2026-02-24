<script lang="ts">
	import { loadCode } from '../../output-es/App.LoadFigure';
	import { onMount } from 'svelte';

	let { name, src } = $props();

	onMount(() => {
		loadCode(src)();
	});
</script>

<details>
	<summary>{name}.fld</summary>
	<div id="codemirror-{name}"></div>
</details>
