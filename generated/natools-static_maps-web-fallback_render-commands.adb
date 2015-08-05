with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Fallback_Render.Commands is

   P : constant array (0 .. 3) of Natural :=
     (1, 4, 8, 9);

   T1 : constant array (0 .. 3) of Unsigned_8 :=
     (14, 28, 24, 26);

   T2 : constant array (0 .. 3) of Unsigned_8 :=
     (18, 19, 18, 29);

   G : constant array (0 .. 34) of Unsigned_8 :=
     (0, 0, 0, 9, 0, 3, 6, 0, 5, 10, 0, 1, 2, 0, 6, 3, 0, 0, 0, 0, 0, 4, 13,
      8, 0, 0, 0, 0, 0, 2, 0, 8, 1, 3, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 35;
         F2 := (F2 + Natural (T2 (K)) * J) mod 35;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 17;
   end Hash;

end Natools.Static_Maps.Web.Fallback_Render.Commands;
