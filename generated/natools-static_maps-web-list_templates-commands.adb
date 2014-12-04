with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.List_Templates.Commands is

   P : constant array (0 .. 4) of Natural :=
     (1, 4, 8, 9, 10);

   T1 : constant array (0 .. 4) of Unsigned_8 :=
     (3, 19, 10, 24, 26);

   T2 : constant array (0 .. 4) of Unsigned_8 :=
     (17, 24, 18, 22, 17);

   G : constant array (0 .. 28) of Unsigned_8 :=
     (0, 0, 0, 0, 11, 0, 7, 4, 9, 0, 0, 5, 0, 0, 4, 11, 8, 13, 0, 7, 0, 0,
      0, 11, 0, 0, 1, 0, 2);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 29;
         F2 := (F2 + Natural (T2 (K)) * J) mod 29;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 14;
   end Hash;

end Natools.Static_Maps.Web.List_Templates.Commands;
