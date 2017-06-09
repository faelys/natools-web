with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Fallback_Render.Commands is

   P : constant array (0 .. 4) of Natural :=
     (1, 4, 9, 13, 16);

   T1 : constant array (0 .. 4) of Unsigned_8 :=
     (35, 14, 45, 40, 25);

   T2 : constant array (0 .. 4) of Unsigned_8 :=
     (61, 39, 15, 15, 1);

   G : constant array (0 .. 62) of Unsigned_8 :=
     (28, 0, 0, 5, 8, 23, 0, 0, 0, 24, 9, 0, 28, 23, 0, 26, 2, 19, 0, 0, 27,
      22, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 14, 0, 11, 13, 12, 12, 7, 0, 7,
      0, 0, 0, 0, 0, 0, 8, 4, 15, 0, 0, 0, 8, 0, 3, 11, 0, 9, 14, 25, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 63;
         F2 := (F2 + Natural (T2 (K)) * J) mod 63;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 31;
   end Hash;

end Natools.Static_Maps.Web.Fallback_Render.Commands;
