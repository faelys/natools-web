with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Fallback_Render.Commands is

   P : constant array (0 .. 2) of Natural :=
     (1, 4, 9);

   T1 : constant array (0 .. 2) of Unsigned_8 :=
     (2, 10, 21);

   T2 : constant array (0 .. 2) of Unsigned_8 :=
     (0, 21, 17);

   G : constant array (0 .. 30) of Unsigned_8 :=
     (0, 0, 12, 0, 0, 0, 0, 11, 0, 0, 0, 0, 11, 1, 0, 12, 0, 0, 7, 8, 0, 14,
      0, 12, 1, 9, 5, 6, 6, 0, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 31;
         F2 := (F2 + Natural (T2 (K)) * J) mod 31;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 15;
   end Hash;

end Natools.Static_Maps.Web.Fallback_Render.Commands;
