<script lang="ts">
let {
		children,
		showDataPane,
		doubleSize = false,
      textPaneWidth = doubleSize ? `600px` : 'var(--text-pane-width)'
	} = $props();
</script>

<div
   id="grid"
   class={`grid-container
      ${!showDataPane ? 'data-pane-hidden' : ''}
      ${doubleSize ? 'double-size' : ''}`}
   style={showDataPane
      ? `--text-pane-width: ${textPaneWidth}; grid-template-columns: auto var(--toggle-button-width) var(--text-pane-width);`
      : `--text-pane-width: ${textPaneWidth}; grid-template-columns: 0 var(--toggle-button-width) var(--text-pane-width);`
   }
>
	{@render children()}
</div>

<style>
   .grid-container {
      display: grid;
      justify-content: center;
   }

   .grid-container.double-size {
      max-width: 66.7vw;
   }

   @media (max-width: 900px) {
      .grid-container {
         display: flex;
         flex-direction: column;
         align-items: center;
         padding: 0 1em;
      }

      .grid-container > :global(*) {
         max-width: 100%;
         width: 100%;
      }

      .grid-container.double-size {
         max-width: 100%;
      }
   }
</style>
