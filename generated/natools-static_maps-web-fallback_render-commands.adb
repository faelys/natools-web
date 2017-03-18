with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Fallback_Render.Commands is

   P : constant array (0 .. 3) of Natural :=
     (3, 4, 8, 9);

   T1 : constant array (0 .. 3) of Unsigned_8 :=
     (31, 29, 11, 40);

   T2 : constant array (0 .. 3) of Unsigned_8 :=
     (29, 31, 23, 18);

   G : constant array (0 .. 42) of Unsigned_8 :=
     (0, 0, 12, 0, 12, 0, 0, 0, 4, 0, 17, 0, 0, 14, 4, 15, 0, 0, 0, 0, 18,
      0, 1, 0, 14, 0, 3, 0, 14, 14, 2, 0, 7, 0, 0, 19, 13, 0, 17, 0, 0, 20,
      11);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 43;
         F2 := (F2 + Natural (T2 (K)) * J) mod 43;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 21;
   end Hash;

end Natools.Static_Maps.Web.Fallback_Render.Commands;
