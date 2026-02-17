import BristolLogo from '$lib/assets/image/bristol.png';
import ICCSLogo from '$lib/assets/image/iccs.png';
import SchmidtLogo from '$lib/assets/image/schmidt.png';
import TuringLogo from '$lib/assets/image/turing.jpg';

export const funders = [
	{
		url: 'https://www.turing.ac.uk/',
		name: 'The Alan Turing Institute',
		logo: TuringLogo
	},
	{
		url: 'https://plrg-bristol.github.io/',
		name: 'University of Bristol',
		logo: BristolLogo
	},
	{
		url: 'https://iccs.cam.ac.uk/',
		name: 'Institute of Computing for Climate Science',
		logo: ICCSLogo
	},
	{
		url: 'https://www.schmidtsciences.org/',
		name: 'Schmidt Sciences',
		logo: SchmidtLogo
	}
] as const;

export const contributors = [
	{
		url: 'https://github.com/JosephBond',
		name: 'Joe Bond',
		organisation: 'University of Bristol'
	},
	{
		url: 'https://github.com/hafniz',
		name: 'Haofei Chen',
		organisation: 'University of Edinburgh'
	},
	{
		url: 'https://github.com/ColinC-UoE',
		name: 'Colin Crawford',
		organisation: 'University of Edinburgh'
	},
	{
		url: 'https://cristina-david.github.io/',
		name: 'Cristina David',
		organisation: 'University of Bristol'
	},
	{
		url: 'https://github.com/T-ab-F',
		name: 'Thomas Frith',
		organisation: 'University of Cambridge'
	},
	{
		url: 'https://github.com/harleengulati03',
		name: 'Harleen Gulati',
		organisation: 'University of Bristol'
	},
	{
		url: 'https://github.com/hanaizakim',
		name: 'Hana Iza Kim',
		organisation: 'University of Cambridge'
	},
	{
		url: 'https://min-nguyen.github.io/',
		name: 'Minh Nguyen',
		organisation: 'University of Bristol'
	},
	{
		url: 'https://dorchard.github.io/',
		name: 'Dominic Orchard',
		organisation: 'University of Kent, University of Cambridge'
	},
	{ url: 'https://github.com/jacobpake', name: 'Jacob Pake', organisation: '' },
	{
		url: 'https://dynamicaspects.org/research/',
		name: 'Roly Perera',
		organisation: 'University of Cambridge, University of Bristol'
	},
	{
		url: 'https://tomasp.net/',
		name: 'Tomas Petricek',
		organisation: 'Charles University'
	},
	{
		url: 'https://alfy91.github.io/',
		name: 'Alfonso Piscitelli',
		organisation: 'University of Salerno'
	},
	{
		url: 'https://a-ch.in/ty-a',
		name: 'Achintya Rao',
		organisation: 'UWE Bristol'
	},
	{
		url: 'https://mengwangoxf.github.io/',
		name: 'Meng Wang',
		organisation: 'University of Bristol'
	}
];
