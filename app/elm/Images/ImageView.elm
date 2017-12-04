module Images.ImageView exposing (imageView)

import Html exposing (Html, div, img, text, p, strong, span, a)
import Html.Attributes exposing (src, alt, title, class, href)
import Images.Models exposing (Image, Person, PersonId)
import Routing exposing (personPath, imagePath, personImagePath)


type NavDirection
    = Previous
    | Next


imageView : ( Image, List Image ) -> ( Maybe Person, List Person ) -> Html msg
imageView ( image, images ) ( maybePerson, people ) =
    div [ class "image-details" ]
        [ renderImage image
        , renderImageDescription image
        , renderPeopleList people maybePerson
        , renderImageNav image images maybePerson
        ]


prevImage : Image -> List Image -> Maybe Image
prevImage image gallery =
    gallery
        |> List.filter (\img -> (img.id < image.id))
        |> List.reverse
        |> List.head


nextImage : Image -> List Image -> Maybe Image
nextImage image gallery =
    gallery
        |> List.filter (\img -> (img.id > image.id))
        |> List.head


renderImageNavLink : Maybe Image -> Maybe Person -> NavDirection -> Html msg
renderImageNavLink maybeImage maybePerson direction =
    case maybeImage of
        Just image ->
            renderImageLink image maybePerson direction

        Nothing ->
            text "Nothing"


renderImageNav : Image -> List Image -> Maybe Person -> Html msg
renderImageNav image gallery maybePerson =
    div []
        [ renderImageNavLink (prevImage image gallery) maybePerson Previous
        , renderImageNavLink (nextImage image gallery) maybePerson Next
        ]


renderImage : Image -> Html msg
renderImage image =
    img
        [ src image.fullsize
        , alt image.description
        , title image.description
        ]
        []


renderImageDescription : Image -> Html msg
renderImageDescription image =
    p []
        [ text image.description ]


renderPeopleList : List Person -> Maybe Person -> Html msg
renderPeopleList people maybePerson =
    p [ class "people" ]
        [ strong []
            [ text "People: " ]
        , span []
            (people
                |> List.map (\person -> (renderPersonLink person maybePerson))
            )
        ]


renderPersonLink : Person -> Maybe Person -> Html msg
renderPersonLink person maybeCurrentPerson =
    let
        path =
            personPath person.id
    in
        case maybeCurrentPerson of
            Just currentPerson ->
                if currentPerson.id == person.id then
                    text person.name
                else
                    a
                        [ href path ]
                        [ text person.name ]

            Nothing ->
                a
                    [ href path ]
                    [ text person.name ]


renderImageLink : Image -> Maybe Person -> NavDirection -> Html msg
renderImageLink image maybePerson direction =
    case maybePerson of
        Just person ->
            a
                [ href (personImagePath image.id person.id) ]
                [ text (toString direction) ]

        Nothing ->
            a
                [ href (imagePath image.id) ]
                [ text (toString direction) ]
