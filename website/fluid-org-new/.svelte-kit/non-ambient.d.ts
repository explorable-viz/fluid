
// this file is generated — do not edit it


declare module "svelte/elements" {
	export interface HTMLAttributes<T> {
		'data-sveltekit-keepfocus'?: true | '' | 'off' | undefined | null;
		'data-sveltekit-noscroll'?: true | '' | 'off' | undefined | null;
		'data-sveltekit-preload-code'?:
			| true
			| ''
			| 'eager'
			| 'viewport'
			| 'hover'
			| 'tap'
			| 'off'
			| undefined
			| null;
		'data-sveltekit-preload-data'?: true | '' | 'hover' | 'tap' | 'off' | undefined | null;
		'data-sveltekit-reload'?: true | '' | 'off' | undefined | null;
		'data-sveltekit-replacestate'?: true | '' | 'off' | undefined | null;
	}
}

export {};


declare module "$app/types" {
	export interface AppTypes {
		RouteId(): "/" | "/contributors" | "/convolution-wrapped" | "/convolution" | "/faq" | "/moving-average" | "/student-projects";
		RouteParams(): {
			
		};
		LayoutParams(): {
			"/": Record<string, never>;
			"/contributors": Record<string, never>;
			"/convolution-wrapped": Record<string, never>;
			"/convolution": Record<string, never>;
			"/faq": Record<string, never>;
			"/moving-average": Record<string, never>;
			"/student-projects": Record<string, never>
		};
		Pathname(): "/" | "/contributors" | "/contributors/" | "/convolution-wrapped" | "/convolution-wrapped/" | "/convolution" | "/convolution/" | "/faq" | "/faq/" | "/moving-average" | "/moving-average/" | "/student-projects" | "/student-projects/";
		ResolvedPathname(): `${"" | `/${string}`}${ReturnType<AppTypes['Pathname']>}`;
		Asset(): "/dataset/methane-emissions.json" | "/dataset/non-renewables.json" | "/dataset/renewables-new.json" | "/fluid/convolution/emboss.fld" | "/fluid/convolution/testImage.fld" | "/fluid/convolution-wrapped.fld" | "/fluid/convolution.fld" | "/fluid/lib/graphics.fld" | "/fluid/lib/matrix.fld" | "/fluid/lib/prelude.fld" | "/fluid/lib/stats.fld" | "/fluid/methane.fld" | "/fluid/moving-average.fld" | "/fluid/non-renewables.fld" | "/fluid/nonRenewables.fld" | "/fluid-poster.pdf" | "/pdf/Turing-2023.09.19.pdf" | "/robots.txt" | string & {};
	}
}