import React, { Component } from 'react';
import PropTypes from 'prop-types';
import TfEditor from '../../BottomBar/Editor/TfEditor'
import { connect } from 'react-redux';
import { addEnvelope, deleteEnvelope, clearEnvelopes, addPoint, addTransferFunction} from '../../BottomBar/Editor/actions';
import DataManager from '../../../api/DataManager';
import Picker from '../../BottomBar/Picker';
import styles from '../../BottomBar/Editor/TfEditor.scss'

class TransferFunctionEditor extends Component {
  constructor(props) {
    super(props);
    this.state = {
      volumeName : this.props.volumeName
    }
    this.convertPointsBeforeSending = this.convertPointsBeforeSending.bind(this);
    this.convertEnvelopesFromBackendToGUI = this.convertEnvelopesFromBackendToGUI.bind(this);
    this.convertHistogramFromBackendToGUI = this.convertHistogramFromBackendToGUI.bind(this);
  }

  componentDidMount() {
    this.updateTransferfunctionState(this.props.properties);
  }

  componentDidUpdate(prevProps, prevState) {
    if (this.props.transferfunction !== prevProps.transferfunction) {
      this.sendEnvelopes();
    }
  }

  convertEnvelopesFromBackendToGUI(data) {
    if(data === undefined || data === "") {
      return undefined;
    }
    else {
      var envelopes = (eval('('+data+')'));
      if (Object.keys(envelopes).length != 0 || envelopes.constructor != Object) {
        this.props.ClearEnvelopes(this.state.volumeName);
        envelopes.map(envelope => {
          envelope = envelope['points'].map(point =>
              Object.assign({},
                  { color : point.color,
                    position : {
                      x : point.position.x * 800,
                      y : 600 - point.position.y * 600,
                    },
                  })
            )
            console.log(envelope)
            this.props.AddEnvelope(envelope, this.state.volumeName)
          }
        )
      }
    }
  }

  convertHistogramFromBackendToGUI(data) {
    if(data !== undefined && data !== "") {
      var convertedData = (eval('('+data+')'));
      return convertedData;
    }
  }

  updateTransferfunctionState(data) {
    let transferfunctiondata = {};
    var envelopes;
    data.map(property => {
          switch(property.Description.Name) {
          case "TransferFunction": {
          transferfunctiondata[property.Description.Name] = Object.assign({},
            {
              URI: property.Description.Identifier,
            }
           )
          envelopes = property.Value;
          break;
          }
          case "Histogram": {
            transferfunctiondata[property.Description.Name] = Object.assign({},
              {
                URI: property.Description.Identifier,
                value: this.convertHistogramFromBackendToGUI(property.Value)
              }
            )
            break;
          }
          case "MinValue":
          case "MaxValue": {
            transferfunctiondata[property.Description.Name] = Object.assign({},
              {
                URI: property.Description.Identifier,
                value: Number(property.Value)
              }
            )
            break;
          }
          default: {
            transferfunctiondata[property.Description.Name] = Object.assign({},
              {
                URI: property.Description.Identifier,
                value: property.Value
              }
            )
            break;
          }
        }
    })

    var transferfunction = {
      id: this.state.volumeName,
      data: transferfunctiondata
    };

    this.props.AddTransferFunction(transferfunction);
    this.convertEnvelopesFromBackendToGUI(envelopes);
  }

  convertPointsBeforeSending(position) {
      let x = (position.x);
      let y = (600 - position.y);
      return {x: x, y: y};
  }

  keepCloning(objectpassed) {
    if (objectpassed === null || typeof objectpassed !== 'object') {
       return objectpassed;
    }
    // give temporary-storage the original obj's constructor
    var temporaryStorage = objectpassed.constructor();
    for (var key in objectpassed) {
      temporaryStorage[key] = this.keepCloning(objectpassed[key]);
    }
    return temporaryStorage;
  }

  sendEnvelopes() {
    var envelopes = this.keepCloning(this.props.transferfunction.data.TransferFunction.envelopes);
    envelopes = envelopes.map(envelope =>
      Object.assign({},
        {points: envelope.points.map(point =>
            Object.assign({},
            { color : point.color,
              position : this.convertPointsBeforeSending(point.position),
            })
          )
        },
        {height:600},
        {width: 800},
      )
    )
    DataManager.setValue(this.state.volumeName + ".renderable.TransferFunctionHandler.TransferFunction", JSON.stringify(envelopes));
  }

  render() {
    return(
      <Picker className={( styles.Active )}>
        <p>Transfer function editor</p>
      </Picker>
    );
  }
}

const mapStateToProps = (state, ownProps) => {
    var indexForVolumeName = ownProps.properties[0].Description.Identifier.indexOf('.');
    var volumeName = ownProps.properties[0].Description.Identifier.substring(0, indexForVolumeName != -1 ?
      indexForVolumeName : ownProps.properties[0].Description.Identifier.length);
    var transferfunction;
    state.transferfunctions.map(element => {
        if (element.id === volumeName)
          transferfunction = element;
      })
    return {
      transferfunction,
      volumeName
    }
};

const mapDispatchToProps = (dispatch) => {
  return {
    AddTransferFunction: (transferfunction) => {
      dispatch(addTransferFunction(transferfunction));
    },
    ClearEnvelopes: (activeVolume) => {
      dispatch(clearEnvelopes(activeVolume));
    },
    AddEnvelope: (envelope, activeVolume) => {
      dispatch(addEnvelope(envelope, activeVolume));
    },
  }
}

TransferFunctionEditor = connect(
  mapStateToProps,
  mapDispatchToProps,
  )(TransferFunctionEditor)

export default TransferFunctionEditor;
