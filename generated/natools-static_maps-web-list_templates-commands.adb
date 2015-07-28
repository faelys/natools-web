with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.List_Templates.Commands is

   P : constant array (0 .. 4) of Natural :=
     (3, 6, 8, 9, 10);

   T1 : constant array (0 .. 4) of Unsigned_8 :=
     (21, 1, 26, 12, 43);

   T2 : constant array (0 .. 4) of Unsigned_8 :=
     (29, 3, 44, 7, 13);

   G : constant array (0 .. 44) of Unsigned_8 :=
     (0, 0, 0, 0, 0, 0, 0, 16, 0, 0, 0, 9, 0, 3, 5, 11, 0, 17, 3, 3, 16, 3,
      20, 7, 12, 0, 0, 12, 0, 0, 0, 0, 2, 6, 0, 5, 0, 0, 0, 1, 2, 0, 1, 13,
      0);

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

end Natools.Static_Maps.Web.List_Templates.Commands;
