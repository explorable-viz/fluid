import { writeFileSync } from "fs";
export const writeFileUnsafe = (name) => (str) => writeFileSync(name, str);

export const exitUnsafe = () => process.exit(0);

export const logUnsafe = (str) => console.log(str);

export const logErrorUnsafe = (str) => console.error(str);
