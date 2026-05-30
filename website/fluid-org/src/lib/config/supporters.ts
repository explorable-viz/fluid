import BristolLogo from '$lib/assets/image/bristol.png';
import ICCSLogo from '$lib/assets/image/iccs.png';
import KentLogo from '$lib/assets/image/kent.jpg';
import SchmidtLogo from '$lib/assets/image/schmidt.png';
import TuringLogo from '$lib/assets/image/turing.jpg';

export const supporters = [
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
	},
	{
		url: 'https://www.kent.ac.uk/school-of-computing',
		name: 'University of Kent',
		logo: KentLogo
	}
] as const;
