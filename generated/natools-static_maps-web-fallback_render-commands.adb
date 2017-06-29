with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Fallback_Render.Commands is

   P : constant array (0 .. 4) of Natural :=
     (1, 4, 9, 13, 16);

   T1 : constant array (0 .. 4) of Unsigned_8 :=
     (34, 66, 52, 35, 47);

   T2 : constant array (0 .. 4) of Unsigned_8 :=
     (5, 26, 36, 26, 50);

   G : constant array (0 .. 66) of Unsigned_8 :=
     (0, 11, 0, 0, 2, 21, 0, 28, 16, 0, 0, 0, 0, 15, 0, 5, 0, 0, 12, 30, 0,
      8, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0, 0, 0, 19, 0, 0, 27, 0, 17, 11, 0, 13,
      0, 4, 18, 23, 20, 9, 0, 0, 27, 0, 14, 0, 0, 30, 6, 25, 25, 0, 4, 5, 0,
      2, 14, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 67;
         F2 := (F2 + Natural (T2 (K)) * J) mod 67;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 33;
   end Hash;

end Natools.Static_Maps.Web.Fallback_Render.Commands;
