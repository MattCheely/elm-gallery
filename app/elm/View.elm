module View exposing (..)

import Http
import Models exposing (..)
import Routing
import Messages exposing (Msg)
import Images.Models exposing (Image, Person, PersonId)
import Images.CardView exposing (cardView)
import Images.ImageView exposing (imageView)
import Html exposing (Html, div, text, program)
import Html.Attributes exposing (class)
import RemoteData exposing (WebData)


view : Model -> Html Msg
view model =
    div []
        [ page model ]


remoteDataView : (( List Image, List Person ) -> Html Msg) -> Model -> Html Msg
remoteDataView subview model =
    case (RemoteData.append model.allImages model.allPeople) of
        RemoteData.NotAsked ->
            loadingView

        RemoteData.Loading ->
            loadingView

        RemoteData.Success data ->
            subview data

        RemoteData.Failure err ->
            errorView err


page : Model -> Html Msg
page model =
    case model.route of
        Routing.ImageRoute id ->
            remoteDataView
                (\( allImages, allPeople ) ->
                    case (getImage allImages id) of
                        Just image ->
                            let
                                people =
                                    (getPeople allPeople image)
                            in
                                imageView
                                    ( image, allImages )
                                    ( Maybe.Nothing, people )

                        Nothing ->
                            imageNotFoundView
                )
                model

        Routing.PersonRoute id ->
            remoteDataView
                (\( allImages, allPeople ) ->
                    let
                        maybePerson =
                            (getPerson allPeople id)
                    in
                        case maybePerson of
                            Just person ->
                                let
                                    galleryImages =
                                        (getImagesOfPerson person allImages)
                                in
                                    galleryView
                                        galleryImages
                                        maybePerson

                            Nothing ->
                                personNotFoundView
                )
                model

        Routing.PersonImageRoute personId imageId ->
            remoteDataView
                (\( allImages, allPeople ) ->
                    let
                        maybePerson =
                            (getPerson allPeople personId)
                    in
                        case maybePerson of
                            Just person ->
                                case (getImage allImages imageId) of
                                    Just image ->
                                        let
                                            images =
                                                (getImagesOfPerson person allImages)

                                            people =
                                                (getPeople allPeople image)
                                        in
                                            imageView
                                                ( image, images )
                                                ( maybePerson, people )

                                    Nothing ->
                                        personImageNotFoundView

                            Nothing ->
                                personNotFoundView
                )
                model

        Routing.HomeRoute ->
            remoteDataView
                (\( allImages, allPeople ) ->
                    galleryView allImages Maybe.Nothing
                )
                model

        Routing.PersonNotFound ->
            personNotFoundView

        Routing.ImageNotFound ->
            imageNotFoundView

        Routing.NotFoundRoute ->
            (notFoundView "Page not found")


loadingView : Html Msg
loadingView =
    div
        [ class "loading" ]
        [ text "Loading image data... " ]


errorView : Http.Error -> Html Msg
errorView err =
    div
        [ class "error" ]
        [ text ("There was an error... " ++ (toString err)) ]


galleryView : List Image -> Maybe Person -> Html Msg
galleryView images maybePerson =
    div
        [ class "gallery" ]
        (images
            |> List.map (\image -> (cardView image maybePerson))
        )


imageNotFoundView : Html Msg
imageNotFoundView =
    (notFoundView "Image not found.")


personNotFoundView : Html Msg
personNotFoundView =
    (notFoundView "Person not found.")


personImageNotFoundView : Html Msg
personImageNotFoundView =
    (notFoundView "Image of person not found.")


notFoundView : String -> Html Msg
notFoundView msg =
    div []
        [ text msg ]



-- Helpers


getImage : List Image -> Int -> Maybe Image
getImage images id =
    images
        |> List.filter (\image -> image.id == id)
        |> List.head


getPerson : List Person -> Int -> Maybe Person
getPerson allPeople id =
    allPeople
        |> List.filter (\person -> person.id == id)
        |> List.head


getPeople : List Person -> Image -> List Person
getPeople allPeople image =
    List.filter (\person -> (List.member person.id image.people)) allPeople


getImagesOfPerson : Person -> List Image -> List Image
getImagesOfPerson person allImages =
    List.filter (\image -> (List.member person.id image.people)) allImages
