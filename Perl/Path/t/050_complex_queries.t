#!perl
use strict;
use warnings;

use Sereal::Path;
use Sereal::Encoder qw/encode_sereal/;
use Test::More;
use Test::Deep;
use Data::Dumper;

sub hobodecode {
    return unless defined $_[0];
    open my $fh, "| $^X -Mblib=../Encoder -Mblib=../Decoder ../shared/author_tools/hobodecoder.pl -e" or die $!;
    print $fh $_[0];
    close $fh;
}

# dataset is borrowed from http://jsonpath.googlecode.com/svn/trunk/tests/jsonpath-test-js.html

my @data = (
   {
      "o" => {
         "c d" => "e",
         "a" => "a",
         "b" => "b"
      },
      "p" => [ "\$.a", "\$['a']", "\$.'c d'", "\$.*",        "\$['*']",     "\$[*]"       ],
      "r" => [ ['a'],  ['a'],     ['e'],      ['a','b','e'], ['a','b','e'], ['a','b','e'] ],
   },
   {
      "o" => [ 1, "2", 3.14, 'true', undef ],
      "p" => [
          "\$[0]",
          "\$[4]",
          "\$[*]",
          "\$[-1:]",
      ],
      "r" => [
          [1],
          [undef],
          [ 1, '2', '3.14', 'true', undef ],
          [undef]
      ],
   },
   {
      "o" => {
         "points" => [
            { "y" => -5, "x" => 4, "id" => "i1" },
            { "z" => 1, "x" => -2, "y" => 2, "id" => "i2" },
            { "id" => "i3", "y" => 3, "x" => 8 },
            { "x" => -6, "y" => -1, "id" => "i4" },
            { "z" => 1, "x" => 0, "y" => 2, "id" => "i5" },
            { "x" => 1, "y" => 4, "id" => "i6" }
         ]
      },
      "p" => [ "\$.points[1]",                           "\$.points[4].x", "\$.points[*].x" ],
      "r" => [ [{'x'=>-2, 'id'=>'i2', 'y'=>2, 'z'=> 1}], [0],              [4,-2,8,-6,0,1]  ],
         #"\$.points[?(@.id=='i4')].x",
         #"\$['points'][?(@.x*@.x+@.y*@.y > 50)].id",
         #"\$.points[?(@.z)].id",
         #"\$.points[(@.length-1)].id"
   },
   {
      "o" => {
         "menu" => {
            "header" => "SVG Viewer",
            "items" => [
               { "id" => "Open" },
               { "label" => "Open New", "id" => "OpenNew" },
               undef,
               { "id" => "ZoomIn", "label" => "Zoom In" },
               { "label" => "Zoom Out", "id" => "ZoomOut" },
               { "label" => "Original View", "id" => "OriginalView" },
               undef,
               { "id" => "Quality" },
               { "id" => "Pause" },
               { "id" => "Mute" },
               undef,
               { "id" => "Find", "label" => "Find..." },
               { "label" => "Find Again", "id" => "FindAgain" },
               { "id" => "Copy" },
               { "label" => "Copy Again", "id" => "CopyAgain" },
               { "id" => "CopySVG", "label" => "Copy SVG" },
               { "id" => "ViewSVG", "label" => "View SVG" },
               { "label" => "View Source", "id" => "ViewSource" },
               { "id" => "SaveAs", "label" => "Save As" },
               undef,
               { "id" => "Help" },
               { "label" => "About Adobe CVG Viewer...", "id" => "About" }
            ]
         }
      },
      "p" => [
          #!!! "\$..[0]"
          #"\$.menu.items[?(@ && @.id && !@.label)].id",
          #"\$.menu.items[?(@ && @.label && /SVG/.test(@.label))].id",
          #"\$.menu.items[?(!@)]",
      ],
      "r" => [
          [{ 'id'=>'Open' }]
      ],
   },
   {
      "o" => {
         "b" => [ 5, 6, 7, 8 ],
         "a" => [ 1, 2, 3, 4 ]
      },
      "p" => [
          #!!! "\$..[0]"
          #"\$..[-1:]",
          #"\$..[?(@%2==0)]",
      ],
      "r" => [
          [1,5]
      ],
   },
   {
      "o" => {
         "arc" => { "dphi" => 120, "color" => "green", "y" => 4, "x" => 2, "r" => 2, "phi0" => 30 },
         "lin" => { "x" => 2, "y" => 3, "color" => "red" },
         "cir" => { "r" => 1, "color" => "blue", "y" => 2, "x" => 5 },
         "pnt" => { "x" => 0, "y" => 7 }
      },
      "p" => [
          #!!! "\$['lin','cir'].color"
          #"\$.'?(@.color)'.x",
      ],
      "r" => [
          ['red','blue']
      ],
   },
   {
      "o" => {
         "arc" => { "phi0" => 30, "r" => 2, "x" => 2, "color" => "green", "y" => 4, "dphi" => 120 },
         "pnt" => { "y" => 7, "x" => 0 },
         "cir" => { "r" => 1, "x" => 5, "color" => "blue", "y" => 2 },
         "lin" => { "x" => 2, "y" => 3, "color" => "red" }
      },
      "p" => [
          #"\$['lin','arc'].color"
          #"\$.'?(@.color)'.x",
          #"\$[lin,arc].color"
      ],
      "r" => [
          ['red','green']
      ],
   },
   # {
      # "o" => {
         # "text" => [
            # "hello",
            # "world2.0"
         # ]
      # },
      # "p" => [
         # #"\$.text[?(@.length > 5)]",
         # #"\$.text[?(@.charAt(0) == 'h')]"
      # ]
   # },
   {
      "o" => {
         "b" => { "b" => 5, "a" => 4 },
         "a" => { "b" => 3, "a" => 2 },
         "c" => {
            "c" => 8,
            "a" => {
               "a" => 6,
               "b" => 7
            }
         }
      },
      "p" => [
      #!!!    "\$..a"
      ],
      "r" => [
          [ { 'a' => 2, 'b' => 3 }, 4, 2, { 'a' => 6, 'b' => 7 }, 6 ]
      ],
   },
   # {
      # "o" => {
         # "a" => [
            # { "a" => 5, "@" => 2, "\$" => 3 },
            # { "a" => 6, "@" => 3, "\$" => 4 },
            # { "a" => 7, "@" => 4, "\$" => 5 }
         # ]
      # },
      # "p" => [
         # #"\$.a[?(@['\\@']==3)]",
         # #"\$.a[?(@['\$']==5)]"
      # ],
   # }
);

my $ignore_order = 1;
foreach (@data) {
    my $p = $_->{p};
    my $r = $_->{r};
    my $sp = Sereal::Path->new(encode_sereal($_->{o}));

    for (my $i = 0; $i < scalar @$p; ++$i) {
        my $query    = $p->[$i];
        my $expected = $r->[$i];
        my $got = [ $sp->values($query) ];

        if ($ignore_order) {
            cmp_bag($got, $expected, "query $query")
                or diag("got:\n" . Dumper($got) . "expected:\n" . Dumper($expected));
        } else {
            is_deeply($got, $expected, "query $query")
                or diag("got:\n" . Dumper($got) . "expected:\n" . Dumper($expected));
        }
    }
}

done_testing();
