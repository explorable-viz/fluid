const absurd = a => {
  const spin = spin$a0$copy => {
    let spin$a0 = spin$a0$copy, spin$c = true, spin$r;
    while (spin$c) {
      const v = spin$a0;
      spin$a0 = v;
    }
    return spin$r;
  };
  return spin(a);
};
export {absurd};
