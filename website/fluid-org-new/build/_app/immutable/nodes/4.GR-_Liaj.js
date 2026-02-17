import{f as k,a as b}from"../chunks/D2M_aK7b.js";import{h as y,f as I,s as a,b as A,g as n,d as D,c as o,r as s,n as C,t as $}from"../chunks/C5WKpRwH.js";import{t as z}from"../chunks/CpBC8Q5X.js";import{C as E}from"../chunks/Ds6RtFwm.js";import{S as N,D as S}from"../chunks/BiMlg1xD.js";import{F as j}from"../chunks/CZcMxXFy.js";import{G}from"../chunks/CcJ7_MrR.js";function x(t,e,p,l){var r=t.__style;if(y||r!==e){var i=z(e);(!y||i!==t.getAttribute("style"))&&(i==null?t.removeAttribute("style"):t.style.cssText=i),t.__style=e}return l}var H=k('<div id="fig-data-pane" class="flex-right-align data-pane"><div id="fig-input" class="data-pane-column"></div> <div id="fig-intermediate" class="data-pane-column"></div></div>'),W=k(`<!> <div class="flex-left-align"><p><span class="highlight">Explorable explanations</span> are interactive web essays that explain
			challenging technical ideas. This elegant <a href="https://distill.pub/2019/computing-receptive-fields/">distill.pub article</a> explains <span class="highlight">matrix convolution</span> and related ideas like <span class="highlight">receptive field</span>, important notions in CNNs that also have
			applications in image processing. Educational efforts like these are valuable but
			labour-intensive, especially for the kind of interactive graphics we might want to show how an
			algorithm like convolution works.</p> <p>How could a language like Fluid help? For an interactive explanation of an algorithm, one
			possibility is to use Fluid's built-in provenance-tracking infrastructure to allow a user to
			explore the relationships between the stages of the convolution pipeline, using interactions
			like the ones shown below. This moves a real implementation closer to being a <span class="highlight">self-explanatory artifact</span>, reducing the need for separate, custom-crafted explanations. Enriched with integrated
			documentation, “open implementations” like these could form the basis of a kind of <span class="highlight">literate execution</span> and way of authoring explorable explanations with
			less effort.</p> <h4>An infrastructure for explorable explanations</h4> <p>As a simple illustration, consider the following Fluid implementation of convolution. The
			program takes an input matrix and transforms it using a small matrix called a filter (or
			kernel), as might be used in image processing to apply an effect like blurring or embossing.
			Toggle the <span class="highlight">data pane</span> on the left to reveal the <code>inputImage</code> and <code>filter</code>; then mouse over the <code>output</code> to see how the inputs are being used.
			This implementation can automatically reveal how different cells in the output demand different
			cells in the input array and filter.</p></div> <!> <div class="flex-left-align"><div class="flex-bottom-align"><!> <div><p class="when-data-pane-visible svelte-1exw3gh">Notice how only certain parts of the input are relevant to a given output cell. Can some
					of the irrelevant inputs be attributed to zeros in the filter? Which ones? Also notice how
					the demand varies as we approach the edge of the output: because this implementation
					treats the input as though it were padded with zeros at the boundary, parts of the filter
					are irrelevant to output cells near the edge. Interactions like these would be more useful
					if we could show the actual computation involved for a given element, rather than just the
					IO relationships, but even this simple extensional view already reveals interesting things
					about the implementation, without the need for a bespoke visualisation.</p> <p class="when-data-pane-visible svelte-1exw3gh">What happens when you mouse over a corner of the output?</p></div></div></div> <div></div> <div></div> <div class="flex-left-align"><h4>Relations of cognacy</h4> <p>The idea of <span class="highlight">related inputs</span> introduced earlier can also be
			informative. Try interacting with the <code>inputImage</code> instead. The highlighted <code>output</code> now shows the elements that consume the data point under your mouse; the highlighted <code>inputImage</code> region includes all the <span class="highlight">cognates</span> of that data point: all the inputs
			that have one of those outputs as an ancestor in the dependence graph. The highlighted region is
			a kind of “light cone”, picking out a causally closed region of the dependence graph.</p> <p>The key take away here is that the author can simply express convolution as a pure functional
			algorithm; the Fluid runtime and visualisation front-end takes care of providing the
			interactions. The library function <code>convolve</code> below implements the convolution
			algorithm, and helper functions <code>zero</code>, <code>wrap</code> and <code>extend</code> implement specific policies (“methods”)
			for dealing with the boundary.</p> <!> <p class="right-justified"><a href="../convolution-wrapped/">continued..</a></p></div>`,1);function U(t){let e=D(!1);const p={fluidSrcPath:["../fluid"],inputs:["inputImage","filter"],query:!1,linking:!1},l="../fluid/convolution.fld",r="../fluid/lib/matrix.fld";G(t,{get showDataPane(){return n(e)},children:(i,q)=>{var d=W(),c=I(d);N(c,{});var u=a(c,4);S(u,{get showDataPane(){return n(e)},toggle:()=>A(e,!n(e)),children:(F,M)=>{var P=H();b(F,P)}});var h=a(u,2),f=o(h),g=o(f);j(g,{get spec(){return p},fld:l});var v=a(g,2),m=o(v),_=a(m,2);s(v),s(f),s(h);var w=a(h,6),T=a(o(w),6);E(T,{name:"matrix",src:r}),C(2),s(w),$(()=>{x(m,n(e)?"":"display: none"),x(_,n(e)?"":"display: none")}),b(i,d)}})}export{U as component};
