with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.List_Templates.Commands is

   P : constant array (0 .. 4) of Natural :=
     (3, 6, 8, 9, 10);

   T1 : constant array (0 .. 4) of Unsigned_8 :=
     (35, 18, 7, 4, 32);

   T2 : constant array (0 .. 4) of Unsigned_8 :=
     (9, 36, 2, 31, 27);

   G : constant array (0 .. 40) of Unsigned_8 :=
     (6, 16, 0, 0, 0, 3, 0, 0, 0, 0, 15, 8, 0, 5, 17, 3, 0, 0, 13, 4, 0, 0,
      0, 0, 16, 0, 0, 0, 0, 0, 1, 7, 0, 11, 7, 0, 2, 3, 9, 4, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 41;
         F2 := (F2 + Natural (T2 (K)) * J) mod 41;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 20;
   end Hash;

end Natools.Static_Maps.Web.List_Templates.Commands;
