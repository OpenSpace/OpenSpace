import React, {Component} from 'react'
import PropTypes from 'prop-types';
import { connect } from 'react-redux';
import EnvelopeCanvas from '../presentational/EnvelopeCanvas'
import styles from '../style/Envelope.scss'
import GraphBody from '../../../common/Graph/GraphBody'
import Point from '../presentational/Point'
import PointPosition from '../presentational/PointPosition'
import { toggleActiveEnvelope, toggleActivePoint, movePoint, setClickablePoint } from '../../../../api/Actions/transferFunctionActions.js';

const hasActiveChild = (envelope) => {
  var hasActiveChild = false;
  envelope.points.forEach(function(point) {
    if (point.active)
      hasActiveChild = true;
  })
  return hasActiveChild;
}

const getPointPositions = (envelopes, height) => {
  let convertedPoints = [];
  if(envelopes.length !== 0) {
    envelopes.map(envelope =>
        envelope.points.map(point =>
          convertedPoints.push(
            Object.assign({},
              {x1: point.position.x,
               y1: point.position.y,
               x2: point.position.x,
               y2: height}
            )
          )
      )
    )
    return convertedPoints;
  }
}

const mapStateToProps = (state, ownProps) => {
  let envelopes, URI, minValue, maxValue, pointPositions;
  envelopes = ownProps.activeVolume.properties.find(function (obj) { return obj.id === "TransferFunction"; }).Value.map(envelope => {
    return {
      ...envelope,
      points: envelope.points.map(point => {
        return {
          ...point,
          position: {
            x: point.position.x * ownProps.width,
            y: ownProps.height - point.position.y * ownProps.height,
          }
        }
      })
    }
  })
  minValue = Number(ownProps.activeVolume.properties.find(function (obj) { return obj.id === "MinValue"; }).Value);
  maxValue = Number(ownProps.activeVolume.properties.find(function (obj) { return obj.id === "MaxValue"; }).Value);

  pointPositions = getPointPositions(envelopes, ownProps.height)
  return {
    envelopes,
    minValue,
    maxValue,
    pointPositions
  };
};

const mapDispatchToProps = (dispatch, ownProps) => {
  return {
    handleDrag: (e, ui, index, envelope) => {

      var id = envelope.points[index].id;
      if(ui.deltaX !== 0 && ui.deltaY !== 0) {
        dispatch(setClickablePoint(false, envelope.id, id, ownProps.URI));
      }

      let deltaPosition = {
        x: ui.deltaX / ownProps.width,
        y: 0,
      }

      if(!envelope.points[index].anchor)
        deltaPosition.y = - ui.deltaY / ownProps.height;

      dispatch(movePoint(deltaPosition, id, envelope.id, ownProps.URI));
    },
    handleClick: (envelope, pointId) => {
      if (envelope.points[pointId].clickable === false) {
        dispatch(setClickablePoint(true, envelope.id, pointId, ownProps.URI));
      }
      else {
        if (envelope.active === true || hasActiveChild(envelope)) {
          dispatch(toggleActivePoint(envelope.id, pointId, ownProps.URI));
        }
        else {
          dispatch(toggleActiveEnvelope(envelope.id, ownProps.URI));
        }
      }
    }
  }
}

const Envelope = connect(
  mapStateToProps,
  mapDispatchToProps,
  )(EnvelopeCanvas)

export default Envelope;
