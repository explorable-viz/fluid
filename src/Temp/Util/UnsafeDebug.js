import { writeFileSync } from "fs";
export const writeFileUnsafe = (name) => (str) => writeFileSync(name, str);

export const exitUnsafe = () => process.exit(0);
