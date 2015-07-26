with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.List_Templates.Commands is

   P : constant array (0 .. 4) of Natural :=
     (3, 6, 8, 9, 10);

   T1 : constant array (0 .. 4) of Unsigned_8 :=
     (22, 41, 39, 42, 16);

   T2 : constant array (0 .. 4) of Unsigned_8 :=
     (27, 10, 36, 5, 41);

   G : constant array (0 .. 42) of Unsigned_8 :=
     (0, 2, 0, 0, 4, 0, 12, 0, 0, 5, 18, 0, 16, 0, 18, 0, 10, 11, 0, 0, 0,
      0, 8, 0, 16, 0, 13, 0, 0, 0, 0, 8, 7, 10, 0, 1, 0, 3, 0, 0, 15, 17,
      20);

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

end Natools.Static_Maps.Web.List_Templates.Commands;
