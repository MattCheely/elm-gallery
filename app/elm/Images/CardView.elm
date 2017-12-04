module Images.CardView exposing (cardView)

import Html exposing (Html, div, img, text, a)
import Html.Attributes exposing (src, href, alt, title, class)
import Images.Models exposing (Image, Person, PersonId)
import Routing exposing (imagePath, personImagePath)


cardView : Image -> Maybe Person -> Html msg
cardView image maybePerson =
    case maybePerson of
        Just person ->
            a
                [ class "view-image"
                , href (personImagePath image.id person.id)
                ]
                [ renderThumbnail image ]

        Nothing ->
            a
                [ class "view-image"
                , href (imagePath image.id)
                ]
                [ renderThumbnail image ]


renderThumbnail : Image -> Html msg
renderThumbnail image =
    div [ class "image" ]
        [ img
            [ src image.thumbnail
            , alt image.description
            , title image.description
            ]
            []
        ]
