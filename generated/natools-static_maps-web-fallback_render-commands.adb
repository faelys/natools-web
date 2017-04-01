with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Fallback_Render.Commands is

   P : constant array (0 .. 3) of Natural :=
     (1, 4, 8, 9);

   T1 : constant array (0 .. 3) of Unsigned_8 :=
     (44, 23, 9, 31);

   T2 : constant array (0 .. 3) of Unsigned_8 :=
     (43, 34, 3, 24);

   G : constant array (0 .. 44) of Unsigned_8 :=
     (0, 9, 11, 0, 1, 0, 1, 0, 0, 14, 0, 0, 0, 11, 0, 16, 9, 0, 0, 10, 0, 0,
      4, 0, 0, 0, 3, 0, 0, 0, 13, 0, 0, 5, 0, 0, 15, 6, 8, 6, 0, 5, 8, 3, 2);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 45;
         F2 := (F2 + Natural (T2 (K)) * J) mod 45;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 22;
   end Hash;

end Natools.Static_Maps.Web.Fallback_Render.Commands;
