# f.luid.org in SvelteKit

A rough clone of `website/fluid-org` in SvelteKit. SvelteKit is a Javascript framework for building/serving sites with many features, but here we are mostly taking advantage of components/templating and the Vite build system, which reduces and chunks up a lot of the Javascript dependencies.

I have made some minor tweaks (e.g. scoping some CSS to components, optimising images and removing external runtime dependencies like Font Awesome).

Loading the homepage produced here requires 1.10MB (398kB compressed) vs 2.08MB (637.09kB) on the current site. This is mostly due to Vite reducing the main Javascript bundle size. Furthermore, the Javascript bundle (and other shared scripts and styles) are not reloaded on navigation within the site.

## Quickstart

```sh
git clone https://github.com/jacobpake/fluid-website-sveltekit
yarn install
yarn run dev
```

This will print a URL to preview the website. The site will automatically refresh if you make any changes inside of the `src` directory.

### Building

Running `yarn run build` will create/update the `build` directory with the static build output. You can publish this anywhere that serves static files or run a preview server with `yarn run preview`.

## Update Fluid

I have included the contents of `output-es` in `src/lib/fluid`. You can update these by building Fluid and directly copying `$FLUID_PATH/output-es/*` into `src/lib/fluid`.

It would be nice to have these included in the Fluid npm module, or to integrate Fluid with the build/dev steps here.

## SvelteKit

This is a SvelteKit website. We are prerendering every page so in the build output there should be no server side functionality.

Svelte has good state management / reactivity, which would be [useful for visualisations](https://datavisualizationwithsvelte.com/#why-svelte-and-d3), but we do not take advantage of this here.

### Routing

Paths are defined using directory names in `src/routes`. The `index.html` equivalent inside a route directory is the `+page.svelte` file. The file `src/routes/convolution/+page.svelte` should produce a HTML response accessible at `https://f.luid.org/convolution`.

### Components

Components define some shared functionality and can be imported and rendered with arguments to other `.svelte` files. For example, the shared `Figure.svelte` components is defined as:

```.svelte
<script lang="ts">
	import { loadFigure } from '$lib/fluid/App.LoadFigure';
	import { onMount } from 'svelte';

	let { spec, fld } = $props();

	onMount(() => {
		loadFigure(spec)('fig')(fld)();
	});
</script>

<div id="fig">
	<div class="fig-loading">loading figure(s)</div>
</div>

<!-- CSS is scoped to the component and served only when
the component is rendered, i.e. not a single big .css file
for the whole site -->
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

```

The component is used in other components or routes like:

```.svelte
<div class="some-other-html">
    <Figure spec={mySpec} fld="example.fld" />
</div>
```

The script block is evaluated during server-side rendering. The `onMount` call ensures that `loadFigure` is not called on the server, only in the browser (otherwise we would run into issues trying to update the DOM in a Node.js process).

If we separated our processing vs rendering work in Fluid, then we could change this to something like:

```.ts
const figure = prepareFigure(spec, fld)

onMount(() => {
  loadFigure(figure, 'fig')
})
```

Here, `figure` would be computed during prerendering, and only the rendering work would be performed by a visitor to the website.

### Assets

Static assets can be kept in the `static` directory and will be served publicly where `static/fluid/non-renewables.fld` will be accessible from `https://f.luid.org/fluid/non-renewables.fld`.

If we prefer, we can keep assets in our `src/lib/assets` directory and import them into components. This enables Vite to do some extra processing, hashing, chunking, adding headers (if running on the server) that support better caching, performance etc. It will even inline them into the HTML if small enough and considered beneficial.

### Further reading and examples

- [Svelte docs](https://svelte.dev/docs/svelte/overview)
- [SvelteKit docs](https://svelte.dev/docs/kit/introduction)
- [Some visualisation examples with Svelte code](https://threlte.xyz/docs/examples/geometry/rendering-points)
- [Another library with examples](https://layercake.graphics/example/CirclePackForce)
- [Prerendered visualisations no JS](https://pancake-charts.surge.sh/)
- [More visualisation example](https://matterviz.janosh.dev/)
- [Live mermaid chart editor](https://mermaid.live)
