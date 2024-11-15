export function intersectionWith_Object (f) {
   return function (m1) {
      return function (m2) {
         var m = {}
         for (var k in m1) {
            if (hasOwnProperty.call(m1, k) && hasOwnProperty.call(m2, k)) {
               m[k] = f(m1[k])(m2[k])
            }
         }
         return m
      }
   }
}
