-- Testing Removing Dups From Node Property List
testImage :: VizImage
testImage =
    VizImage
        { t = VizDiGraph
        , edges =
            [ VizEdge
                { vizSelf =
                    VizNode
                        { vizId = "A"
                        , nodeProps =
                            VizNodeProps
                                { width = 10
                                , height = 10
                                , style = "normal"
                                , shape = "ellipse"
                                }
                        }
                , vizOther =
                    VizNode
                        { vizId = "B"
                        , nodeProps =
                            VizNodeProps
                                { width = 10
                                , height = 10
                                , style = "normal"
                                , shape = "ellipse"
                                }
                        }
                , vizArrowType = VizArrowNormal
                , vizArrow = vizTypeArrow VizDiGraph
                , vizEdgeType = VizEdgeSolid
                , vizDistance = "2"
                , vizColor = VizRed
                }
            , VizEdge
                { vizSelf =
                    VizNode
                        { vizId = "A"
                        , nodeProps =
                            VizNodeProps
                                { width = 10
                                , height = 10
                                , style = "normal"
                                , shape = "ellipse"
                                }
                        }
                , vizOther =
                    VizNode
                        { vizId = "B"
                        , nodeProps =
                            VizNodeProps
                                { width = 10
                                , height = 10
                                , style = "normal"
                                , shape = "ellipse"
                                }
                        }
                , vizArrowType = VizArrowNormal
                , vizArrow = vizTypeArrow VizDiGraph
                , vizEdgeType = VizEdgeSolid
                , vizDistance = "2"
                , vizColor = VizRed
                }
            , VizEdge
                { vizSelf =
                    VizNode
                        { vizId = "A"
                        , nodeProps =
                            VizNodeProps
                                { width = 10
                                , height = 10
                                , style = "normal"
                                , shape = "ellipse"
                                }
                        }
                , vizOther =
                    VizNode
                        { vizId = "B"
                        , nodeProps =
                            VizNodeProps
                                { width = 10
                                , height = 10
                                , style = "normal"
                                , shape = "ellipse"
                                }
                        }
                , vizArrowType = VizArrowNormal
                , vizArrow = vizTypeArrow VizDiGraph
                , vizEdgeType = VizEdgeSolid
                , vizDistance = "2"
                , vizColor = VizRed
                }
            , VizEdge
                { vizSelf =
                    VizNode
                        { vizId = "A"
                        , nodeProps =
                            VizNodeProps
                                { width = 10
                                , height = 10
                                , style = "normal"
                                , shape = "ellipse"
                                }
                        }
                , vizOther =
                    VizNode
                        { vizId = "B"
                        , nodeProps =
                            VizNodeProps
                                { width = 10
                                , height = 10
                                , style = "normal"
                                , shape = "ellipse"
                                }
                        }
                , vizArrowType = VizArrowNormal
                , vizArrow = vizTypeArrow VizDiGraph
                , vizEdgeType = VizEdgeSolid
                , vizDistance = "2"
                , vizColor = VizRed
                }
            ]
        , name = "test"
        , path = "./testOut"
        }
