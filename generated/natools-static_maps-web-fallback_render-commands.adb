with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Fallback_Render.Commands is

   P : constant array (0 .. 3) of Natural :=
     (1, 4, 9, 16);

   T1 : constant array (0 .. 3) of Unsigned_8 :=
     (51, 44, 33, 18);

   T2 : constant array (0 .. 3) of Unsigned_8 :=
     (26, 25, 41, 18);

   G : constant array (0 .. 52) of Unsigned_8 :=
     (0, 0, 7, 0, 0, 23, 2, 2, 0, 0, 0, 25, 25, 7, 0, 0, 0, 21, 0, 0, 0, 0,
      0, 0, 13, 6, 5, 6, 0, 0, 0, 8, 24, 12, 0, 1, 10, 3, 14, 0, 0, 0, 15,
      0, 17, 0, 0, 18, 10, 9, 0, 0, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 53;
         F2 := (F2 + Natural (T2 (K)) * J) mod 53;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 26;
   end Hash;

end Natools.Static_Maps.Web.Fallback_Render.Commands;
