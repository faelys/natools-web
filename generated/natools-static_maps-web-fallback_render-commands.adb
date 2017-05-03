with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Fallback_Render.Commands is

   P : constant array (0 .. 4) of Natural :=
     (1, 4, 9, 13, 16);

   T1 : constant array (0 .. 4) of Unsigned_8 :=
     (45, 7, 25, 10, 41);

   T2 : constant array (0 .. 4) of Unsigned_8 :=
     (25, 20, 45, 58, 55);

   G : constant array (0 .. 60) of Unsigned_8 :=
     (0, 19, 0, 16, 0, 0, 0, 0, 21, 0, 0, 0, 0, 5, 5, 12, 18, 0, 23, 2, 29,
      0, 21, 20, 0, 0, 10, 0, 16, 0, 0, 2, 0, 17, 0, 9, 22, 0, 0, 0, 2, 0,
      14, 1, 3, 0, 3, 15, 0, 22, 17, 13, 0, 0, 6, 0, 7, 0, 0, 0, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 61;
         F2 := (F2 + Natural (T2 (K)) * J) mod 61;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 30;
   end Hash;

end Natools.Static_Maps.Web.Fallback_Render.Commands;
