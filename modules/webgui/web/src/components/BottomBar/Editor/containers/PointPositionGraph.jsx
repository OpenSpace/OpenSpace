import React, {Component} from 'react';
import { connect } from 'react-redux';
import PointPosition from '../presentational/PointPosition';

const preparePoint = (point, endYValue) => {
    return Object.assign({},
        {x1: point.position.x,
         y1: point.position.y,
         x2: point.position.x,
         y2: endYValue}
      )
  }


const getPointPositions = (envelopes, height) => {
    let convertedPoints = [];
    if(envelopes.length !== 0) {
      envelopes.map(envelope =>
          envelope.points.map(point =>
            convertedPoints.push(
              preparePoint(point, height)
            )
        )
      )
      return convertedPoints;
    }
  }

const mapStateToProps = (state, ownProps) => {
  return {
    points: getPointPositions(ownProps.envelopes, ownProps.height)
    };
};

const PointPositionGraph = connect(
  mapStateToProps,
  )(PointPosition)

export default PointPositionGraph;
