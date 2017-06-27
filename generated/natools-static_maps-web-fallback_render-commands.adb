with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Fallback_Render.Commands is

   P : constant array (0 .. 4) of Natural :=
     (1, 4, 9, 13, 16);

   T1 : constant array (0 .. 4) of Unsigned_8 :=
     (36, 11, 1, 37, 53);

   T2 : constant array (0 .. 4) of Unsigned_8 :=
     (14, 48, 64, 12, 13);

   G : constant array (0 .. 64) of Unsigned_8 :=
     (26, 0, 26, 31, 21, 0, 16, 0, 24, 0, 0, 0, 0, 30, 0, 0, 27, 0, 18, 11,
      0, 0, 0, 0, 0, 15, 22, 0, 0, 0, 0, 8, 0, 25, 24, 0, 8, 0, 0, 0, 0, 21,
      0, 1, 9, 0, 2, 7, 6, 18, 0, 12, 0, 0, 0, 5, 0, 0, 15, 27, 0, 4, 0, 2,
      1);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 65;
         F2 := (F2 + Natural (T2 (K)) * J) mod 65;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 32;
   end Hash;

end Natools.Static_Maps.Web.Fallback_Render.Commands;
